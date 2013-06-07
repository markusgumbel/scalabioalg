/*
Copyright 2011 the original author or authors.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package net.gumbix.dynpro

import collection.mutable.ListBuffer
import math.{abs, min, max}
import net.gumbix.dynpro.concurrency.{Stage, Debugger, DynProConfig}
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import scala.Some
import Stage.Stage

/**
 * Convenient class for a two-dimensional index pair (i, j).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
case class Idx(i: Int, j: Int) {
  def +(o: Idx) = Idx(i + o.i, j + o.j)
  def -(o: Idx) = Idx(i - o.i, j - o.j)

  def +(z: Int) = Idx(i + z, j + z)
  def -(z: Int) = Idx(i - z, j - z)

  def +(zi: Int, zj: Int) = Idx(i + zi, j + zj)
  def -(zi: Int, zj: Int) = Idx(i - zi, j - zj)

  def ==(o: Idx) = (i == o.i && j == o.j)

  val MIN = min(i, j)
  val MAX = max(i, j)

  override def toString = "(" + i + ", " + j + ")"
}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 *
 * Current DynPro's structure:
 * The dynpro's implementation of the dynamic programming algorithm is proceeded
 * in 4 stages. Each of this stages can be computed sequentially or concurrently.
 * - Stage i: the creation of an empty two-dimensional matrix.
 * - Stage ii: the evaluation of the cells within this matrix.
 *     This can be done either per row-wise or column-wise.
 * - Stage iii: the conversion of the values into a values interpretable by "Matlab".
 * - Stage iv: the back tracking of the correct path.
 *
 * The computation modes are the following:
 * - NO CONCURRENCY =: 100% sequential
 * - NO DEPENDENCY =: stage ii is sequential , the rest concurrent
 *    stage-wise 25% sequential
 * - CONCURRENCY =: 100% concurrent
 *
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de), Patrick Meppe (tapmeppe@gmail.com)
 */
abstract class DynPro[Decision] extends DynProBasic{

  /********************************************************
  concurrency mode setting - START
  ********************************************************/
  private val minRange = 10

  private var timeMap: Map[Stage, Long] = Map()
  protected def getRecordedTimes = timeMap

  protected final def setCon(dep: ConClass, mode: ConMode): DynProConfig[Decision] =
    setCon(dep, mode, 0, 0, false)

  protected final def setCon(dep: ConClass, mode: ConMode, recordTime: Boolean): DynProConfig[Decision] =
    setCon(dep, mode, 0, 0, recordTime)

  protected final def setCon(dep: ConClass, mode: ConMode, _mxRange: Int, solRange: Int, recordTime: Boolean)
  : DynProConfig[Decision] = {
    val mxRange = if(_mxRange < minRange || m - _mxRange < minRange) m else _mxRange

    new DynProConfig[Decision](dep, mode, mxRange, solRange, recordTime)
  }


  //default setting =: sequential mode, OVERRIDE IF NECESSARY
  //protected val config: DynProConfig[Decision] = setCon(NO_CON, SEQ, 0)

  //debug setting
  protected val config: DynProConfig[Decision] = setCon(LEFT_UP, EVENT)

  /********************************************************
  concurrency mode setting - END
  ********************************************************/

  /********************************************************
  attributes and methods to override - START
  ********************************************************/

  /**
   * matrixForwardPathBackward = true:
   * All possible decisions that lead to state (i, j).
   *
   * matrixForwardPathBackward = false:
   * All possible decisions in state (i, j) that lead to the next state.
   *
   * Note:
   * The array can be empty, meaning that there are no decision to make.
   * This can happen at the boundaries for the matrix.
   */
  def decisions(idx: Idx): Array[Decision]


  /**
   * TODO rename
   * matrixForwardPathBackward = true:
   * The previous state (ip, jp) of state (i, j) if decision d
   * was chosen. So (ip, jp) with decision d leads to i, j.
   *
   * matrixForwardPathBackward = false:
   * The successor state of (i, j) applying decision d. 
   */
  def prevStates(idx: Idx, d: Decision): Array[Idx]


  /**
   * matrixForwardPathBackward = true:
   * A value (e.g. cost) if we go from the previous state to
   * (i, j) and make the decision d.
   *
   * matrixForwardPathBackward = false:
   * A value (e.g. cost) if we make the decision d. 
   */
  def value(idx: Idx, d: Decision): Double


  /**
   * Boundary value: The value in the matrix for not existing states.
   */
  def initValues: Array[Double] = Array(0.0)

  /********************************************************
  attributes and methods to override - END
  ********************************************************/



  /********************************************************
  Main methods (potentially concurrent) - START
  ********************************************************/
  /**
   * matrix containing the accumulated values (costs).
   */
  lazy val matrix: Array[Array[Option[Double]]] = {
    /* Iterate through all the cells. Note that the order depends on the
    problem. Many versions go from top to bottom and left to right.
    However, any other order may also work. */

    /*
    STAGE i of iv
    create an empty n*m matrix. Value{i,j} =: None
    n = mx.length
    m = mx(0).length */
    val empStart = System.nanoTime
    var mx = config.clazz match {
      case NO_CON =>
        val _mx: Array[Array[Option[Double]]] = Array.ofDim(n, m)
        for (i <- 0 until n; j <- 0 until m) {
          _mx(i)(j) = None
        }
        _mx
      case _ => config.emptyMatrix(n, m)
    }
    val empEnd = System.nanoTime

    //CAUTION: Do not merge both blocks

    /*
    STAGE ii of iv
    evaluate the matrix cells sequentially or concurrently
     */
    val mxStart = System.nanoTime
    val toReturn = config.clazz match {
      case NO_CON | NO_DEP | LESS_CON =>
        /* inner method supporting the "calcCellCost" method */
        def getPrevValues(prevIndexes: Array[Idx], prevValues: Array[Double]) = prevValues
        //(Idx, Double) => Unit =: def handleNewValue(idx: Idx, newValue: Double){} //do nothing

        for (k <- 0 until cellsSize)
          mx = calcCellCost(mx, getCellIndex(k))
          //04.06.2013 old version mx = calcCellCost(mx, getCellIndex(k), getPrevValues, (Idx, Double) => Unit)
        mx

      case _ => //LEFT_UP | UP
        // 04.06.2013 old version =: config.computeMatrix(mx, initValues(0), calcCellCost)
        config.computeMatrix(mx, initValues(0), getAccValues, calcNewAccValue)
    }
    val mxEnd = System.nanoTime

    if(config.recordTime){
      timeMap += (Stage.empty -> (empEnd - empStart))
      timeMap += (Stage.matrix -> (mxEnd - mxStart))
    }

    toReturn
  }


  /**
   * TODO Q&D hack for matlab
   * STAGE iii of iv
   * Imporant: lazy, otherwise we get null pointer exceptions.
   */
  lazy val matlabMatrix: Array[Array[Double]] = {
    val start = System.nanoTime
    val toReturn = config.clazz match {
      case NO_CON =>
        val mx: Array[Array[Double]] = Array.ofDim(n, m)
        for (i <- 0 until n; j <- 0 until m) {
          mx(i)(j) = matrix(i)(j) match {
            case a: Some[Double] => a.get
            case _ => 0.0
          }
        }
        mx

      case _ => config.convertMatrix(matrix)
    }
    val end = System.nanoTime

    if(config.recordTime) timeMap += (Stage.matlabMx -> (end - start))

    toReturn
  }


  /**
   * STAGE iv of iv
   * Trace back the path starting from cell (i, j)
   * @param idx
   * @return List of PathEntry
   */
  def solution(idx: Idx): List[PathEntry[Decision]] = {
    //(a:Idx, b:Idx) => false =: def break(startIdx: Idx, cIdx: Idx) = false
    val start = System.nanoTime
    val solution = (config.clazz match {
      case LEFT_UP | UP | NO_DEP =>
        if(config.solRange < minRange || idx.MAX - config.solRange < minRange)
        //seemly concurrent, in reality sequential when the range hasn't been set adequately
          getPathList(idx, matrix, (a:Idx, b:Idx) => false)

        else config.calculateSolution(idx, matrix, getPathList)

      case _ => //NO_CON | LESS_CON, sequential
        getPathList(idx, matrix, (a:Idx, b:Idx) => false)
    }).toList
    val end = System.nanoTime

    if(config.recordTime) timeMap += (Stage.solution -> (end - start))

    if (matrixForwardPathBackward) solution.reverse else solution
  }

  /********************************************************
  Main methods (potentially concurrent) - END
  ********************************************************/


  /********************************************************
  Support methods (sequential & concurrent support) - START
    ********************************************************/
  /**
   *
   * @param idx
   * @param break
   * @return
   */
  private def getPathList(idx: Idx, mx: Array[Array[Option[Double]]],
                          break:(Idx, Idx) => Boolean): ListBuffer[PathEntry[Decision]] = {
    val (eps, listBuffer) = (0.001, new ListBuffer[PathEntry[Decision]]())

    /*
    * Inner function.
    * Note: Requires stack size of -Xss10m
    */
    def calcSI(innerIdx: Idx) {
      // Counter for all possible solutions (might be more than 1):
      //var count = 0
      var solutionFound = false //it makes a lil bit more sense this way
      for (u <- decisions(innerIdx); if !solutionFound) {
        val prevIdx = prevStates(innerIdx, u)
        // v is difference of the current value and the previous values
        // when decision u was made:
        val v = prevIdx match {
          // Empty array:
          case Array() => calcFBack(mx(innerIdx.i)(innerIdx.j).get, initValues, calcG)
          // Non-empty array:
          case indices: Array[Idx] => {
            val args: Array[Double] = for (pidx <- indices) yield {
              mx(pidx.i)(pidx.j) match {
                /*
               case None => throw new RuntimeException("Internal error: " +
                       "Illegal previous state " + pidx + " at " + idx.toString)
                */
                case None => initValues(0)
                case Some(value) => value
              }
            }
            calcFBack(mx(innerIdx.i)(innerIdx.j).get, args, calcG)
          }
        }
        // If v (the difference) is the current value then
        // this decision was made. Note: We may get rounding errors,
        // so we use an epsilon. Also, if we have already found a
        // solution we skip any further solutions:
        if (abs(v - value(innerIdx, u)) < eps) {
          listBuffer += new PathEntry(u, mx(innerIdx.i)(innerIdx.j).get, innerIdx, prevIdx)
          //count += 1
          solutionFound = true
          for (nidx <- prevIdx if !break(idx, innerIdx)) calcSI(nidx)
        }
      }
    }

    calcSI(idx)
    listBuffer
  }


  /**
   * 04.062013 old version
   * @param currentMX
   * @param idx
   * @param getPrevValues
   * @param handleNewValue
   * @return
   */
  private def calcCellCost(currentMX: Array[Array[Option[Double]]],
                                             idx: Idx,
                                             getPrevValues:(Array[Idx], Array[Double]) => Array[Double],
                                             handleNewValue:(Idx, Double) => Unit): Array[Array[Option[Double]]] = {

    // Get a list of values (or empty list):
    val values = for (u <- decisions(idx)) yield {
      prevStates(idx, u) match {
        // Empty array, i.e. there are no prev. states:
        case Array() => calcF(value(idx, u), initValues, calcG)//sum of current and previous values
        // Non-empty array:
        case prevIndexes: Array[Idx] => {
          val prevValues: Array[Double] = for (pidx <- prevIndexes) yield {
            currentMX(pidx.i)(pidx.j) match {
              /*
             case None => throw new RuntimeException("Internal error: " +
                     "Illegal previous state " + pidx + " at " + idx.toString)
              */
              case None => initValues(0)
              case Some(value) => value
            }
          }
          calcF(value(idx, u), getPrevValues(prevIndexes, prevValues), calcG)
          //calcF(value(idx, u), prevValues, calcG)
        }
      }
    }


    // Calculate the new value (min. or max.):
    val newValue: Double = reviseMax(values match {
      case Array() => initValues(0)
      /*a double value, while overriding the initValues method, the dev should keep in mind that
      * the extreme value comes first*/

      case _ => values.toList.reduceLeft(extremeFunction(_, _)) //the maximum or min
    })

    handleNewValue(idx, newValue)

    currentMX(idx.i)(idx.j) = Some(newValue)
    currentMX //return
  }


  private def getAccValues(currentMX: Array[Array[Option[Double]]], idx: Idx,
                           handleNoneState:(Idx) => Unit): Array[Double] = {
    for (u <- decisions(idx)) yield prevStates(idx, u) match {
        // Empty array, i.e. there are no prev. states:
        case Array() => calcF(value(idx, u), initValues, calcG)//sum of current and previous values
        // Non-empty array:
        case prevIndexes: Array[Idx] => {
          val prevValues: Array[Double] = for (pidx <- prevIndexes) yield {
            currentMX(pidx.i)(pidx.j) match {
              /*
              Status (june 2013): This state will be now known as the "none state"
              - sequential
              case None => throw new RuntimeException("Internal error: " +
                      "Illegal previous state " + pidx + " at " + idx.toString)

              - concurrent
              case None => legal and required state

              */
              case None =>
                handleNoneState(pidx)//used in concurrent mode
                initValues(0)

              case Some(value) => value
            }
          }
          calcF(value(idx, u), prevValues , calcG)
          //calcF(value(idx, u), prevValues, calcG)
        }
    }
  }


  /**
   * @param values list of values or empty list in form of an array
   * @return
   */
  private def calcNewAccValue(values: Array[Double]): Option[Double] = {
    // Calculate the new value (min. or max.):
    Some(reviseMax(values match {
      case Array() => initValues(0)
      /*a double value, while overriding the initValues method, the dev should keep in mind that
      * the extreme value comes first*/

      case _ => values.toList.reduceLeft(extremeFunction(_, _)) //the maximum or min
    }))
  }


  private def calcCellCost(currentMX: Array[Array[Option[Double]]],
                           idx: Idx): Array[Array[Option[Double]]] = {
    //(idx) => Unit =: def handleNoneState(idx: Idx){}//do nothing
    currentMX(idx.i)(idx.j) =
      calcNewAccValue(getAccValues(currentMX, idx,(idx) => Unit))

    currentMX
  }

  /********************************************************
  Support methods (sequential & concurrent support) - END
    ********************************************************/

}




// because of Scala bug?!
// solved in build with timestamp 2010 10 19
class IntDynPro extends DynPro[java.lang.Integer] with MatrixPrinter[java.lang.Integer] {
  def n = 1

  def m = 1

  def decisions(idx: Idx) = Array(new Integer(1))

  def prevStates(idx: Idx, d: java.lang.Integer) = Array()

  def value(idx: Idx, d: java.lang.Integer) = 0

  formatter = INT
}
