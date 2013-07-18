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
import concurrency.DynProConfig
import concurrency.ConClass._
import concurrency.ConMode._
import concurrency.Stage._

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
  //default values
  private val (minRange, wuFreq) = (10, 10)

  private var timeMap: Map[Stage, Long] = Map()
  def getDurations = timeMap

  //methods used to configure the "DynPro" computation
  protected final def setConfig(clazz: ConClass, mode: ConMode): DynProConfig[Decision] =
    setConfig(clazz, mode, 0, wuFreq, 0, false)

  protected final def setConfig(clazz: ConClass, mode: ConMode, recordTime: Boolean): DynProConfig[Decision] =
    setConfig(clazz, mode, 0, wuFreq, 0, recordTime)

  protected final def setConfig(clazz: ConClass, mode: ConMode, _mxRange: Int, _bcSize: Int, solRange: Int, recordTime: Boolean)
  : DynProConfig[Decision] = {
    val (mxRange, bcSize) =
      (if(_mxRange < minRange || m - _mxRange < minRange) m else _mxRange,
       clazz match{
         case LEFT_UP => abs(_bcSize)
         case UP => 1 //for now.
         case _ => 0
       })

    new DynProConfig[Decision](
      clazz, mode, recordTime,
      bcSize, () => (n, m), getAccValues, calcCellCost,
      mxRange, convert,
      getExtremeIdx, getPath, solRange
    )
  }


  //default setting =: sequential mode, OVERRIDE IF NECESSARY
  //protected val config: DynProConfig[Decision] = setConfig(LEFT_UP, EVENT)

  //debug setting
  protected val config: DynProConfig[Decision] = setConfig(NO_CON, __)

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


  def updateXY(newX: String, newY: String)

  /********************************************************
  attributes and methods to override - END
  ********************************************************/



  /********************************************************
  Main methods (potentially concurrent) - START
  ********************************************************/
  //this block solely concerns the "matrix" method
  //private val inf = Double.NegativeInfinity
  private var hiddenMatrix: Array[Array[Option[Double]]] = Array.ofDim(1, 1)
  /**
   * STAGE i of ii: costs evaluation
   * 25.06.2013 The prefix had to be changed from "lazy val" to "def"
   * because as from now the same object can apply a dynamic programming algorithm
   * on more than one case.
   * The addition of the private attribute "hiddenMatrix" helps to preserve the properties
   * provided by the "lazy val" prefix for a given case.
   * Meaning the new matrix will be seemly evaluated once and only once
   * for each case
   */
  def getMatrix: Array[Array[Option[Double]]] = {
    /* Iterate through all the cells. Note that the order depends on the
    problem. Many versions go from top to bottom and left to right.
    However, any other order may also work. */
    if(newRound){
      hiddenMatrix = Array.ofDim(n, m)

      val mxStart = System.nanoTime
      config.clazz match {
        case NO_CON | NO_DEP =>
          for (k <- 0 until cellsSize){
            val idx = getCellIndex(k)
            //(idx) => Unit =: def handleNullState(idx: Idx){}//do nothing
            calcCellCost(idx, getAccValues(idx,(idx) => Unit))
          }

        case _ => config.evaluateMatrix //LEFT_UP | UP
      }
      val mxEnd = System.nanoTime

      if(config.recordTime) timeMap += (MATRIX -> (mxEnd - mxStart))
      newRound = false
    }
    hiddenMatrix
  }


  //matlab matrix block
  private var hiddenMatlabMatrix = Array.ofDim[Double](1, 1)
  private def convert(idx: Idx){
    hiddenMatlabMatrix(idx.i)(idx.j) = hiddenMatrix(idx.i)(idx.j) match {
      case value: Some[Double] => value.get
      case _ => 0.0
    }
  }
  /**
   * TODO Q&D hack for matlab
   * Concerning the concurrency this stage has a very low priority.
   * Imporant: lazy, otherwise we get null pointer exceptions.
   */
  def getMatlabMatrix: Array[Array[Double]] = {
    hiddenMatlabMatrix = Array.ofDim(n, m)
    getMatrix

    val start = System.nanoTime
    config.clazz match {
      case NO_CON => for (i <- 0 until n; j <- 0 until m) convert(Idx(i,j))

      case _ => config.convertMatrix
    }
    val end = System.nanoTime
    if(config.recordTime) timeMap += (MATLABMX -> (end - start))
    hiddenMatlabMatrix
  }


  private var newRound = true //new computation round
  private var _idx = Idx(-1, -1)
  /**
   * This method is used to always have an accurate index value.
   * @return
   */
  private def getExtremeIdx = _idx
  /**
   * STAGE iv of iv
   * Trace back the path starting from cell (i, j)
   * @param idx
   * @return List of PathEntry
   */
  def solution(idx: Idx): List[PathEntry[Decision]] = {
    //set the values for the current computation round
    newRound = true
    _idx = idx

    val start = System.nanoTime
    val sol = (config.clazz match {
      case NO_CON => getPath(idx, (a:Idx, b:Idx) => false)

      case _ =>
        if(config.solRange < minRange || idx.MAX - config.solRange < minRange)
        //seemly concurrent, in reality sequential when the range hasn't been set adequately
          getPath(idx, (a:Idx, b:Idx) => false)
        else config.calculateSolution
      //(a:Idx, b:Idx) => false =: def break(startIdx: Idx, cIdx: Idx) = false
    }).toList
    val end = System.nanoTime

    if(config.recordTime){
      val time = end - start
      timeMap += SOLUTION -> (time - timeMap(MATRIX))
      timeMap += TOTAL -> time
    }

    if (matrixForwardPathBackward) sol.reverse else sol

  }

  /********************************************************
  Main methods (potentially concurrent) - END
  ********************************************************/


  /********************************************************
  Support methods (sequential & concurrent support) - START
    ********************************************************/
  /**
   * This method builds the solution path within the given matrix,
   * starting from the given idx
   * @param idx cell within the given matrix
   * @param break limit (end of the solution path)
   * @return
   */
  private def getPath(idx: Idx, break:(Idx, Idx) => Boolean): ListBuffer[PathEntry[Decision]] = {
    getMatrix

    val (eps, pathList) = (0.001, new ListBuffer[PathEntry[Decision]]())
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
          case Array() => calcFBack(hiddenMatrix(innerIdx.i)(innerIdx.j).get, initValues, calcG)
          // Non-empty array:
          case indices: Array[Idx] => {
            val args: Array[Double] = for (pidx <- indices) yield {
              hiddenMatrix(pidx.i)(pidx.j) match {
                /*
               case None => throw new RuntimeException("Internal error: " +
                       "Illegal previous state " + pidx + " at " + idx.toString)
                */
                case Some(value) => value
                case _ => initValues(0)
              }
            }
            calcFBack(hiddenMatrix(innerIdx.i)(innerIdx.j).get, args, calcG)
          }
        }
        // If v (the difference) is the current value then
        // this decision was made. Note: We may get rounding errors,
        // so we use an epsilon. Also, if we have already found a
        // solution we skip any further solutions:
        if (abs(v - value(innerIdx, u)) < eps) {
          pathList += PathEntry(u, hiddenMatrix(innerIdx.i)(innerIdx.j).get, innerIdx, prevIdx)
          //count += 1
          solutionFound = true
          for (nidx <- prevIdx if !break(idx, innerIdx)) calcSI(nidx)
        }
      }
    }

    calcSI(idx)
    pathList
  }


  /**
   * This method is used to get all the possible cost of the given cell
   * @param idx the coordinates of the cell whose cost is about to be calculated.
   * @param handleNullState sort of protocol to follow in case a "None" value occurs in
   *                        concurrent mode.
   * @return Array
   */
  private def getAccValues(idx: Idx, handleNullState:(Idx) => Unit): Array[Double] =
  for (u <- decisions(idx)) yield prevStates(idx, u) match {
      // Empty array, i.e. there are no prev. states:
      case Array() => calcF(value(idx, u), initValues, calcG)//sum of current and previous values
      // Non-empty array:
      case prevIndexes: Array[Idx] => {
        val prevValues: Array[Double] = for (pidx <- prevIndexes) yield {
          /*
          Status (july 2013): This state is now known as the "null state
          */
          hiddenMatrix(pidx.i)(pidx.j) match {
            case Some(value) => value
            case _ => handleNullState(pidx); initValues(0)
          }
        }
        calcF(value(idx, u), prevValues , calcG)
      }
  }


  /**
   * This method selects the most extreme value (cost)
   * out of an array of values (costs)
   * @param values
   * @return
   */
  private def calcCellCost(idx: Idx, values: Array[Double]){
    // Calculate the new value (min. or max.):
    hiddenMatrix(idx.i)(idx.j) = Some(reviseMax(values match {
      case Array() => initValues(0)
      /*a double value, while overriding the initValues method, the dev should keep in mind that
      * the extreme value comes first*/

      case _ => values.toList.reduceLeft(extremeFunction(_, _)) //the maximum or min
    }))
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

  def updateXY(newX: String, newY: String) = {}//do nothing

  formatter = INT
}
