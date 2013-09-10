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
import math.abs
import net.gumbix.dynpro.concurrency.{DynProConfig}
import concurrency.ConClass._
import concurrency.ConMode._
import concurrency.Stage._

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
  private val (minRange, bcMailSize, nanoToSecFact) = (500, 10, 1e9)

  private var timeMap: Map[Stage, Double] = Map()
  def getDurations = timeMap

  //methods used to configure the "DynPro" computation
  protected final def setConfig(clazz: ConClass, mode: ConMode): DynProConfig[Decision] =
    setConfig(clazz, mode, 0, bcMailSize, 0)

  protected final def setConfig(clazz: ConClass, mode: ConMode, solRange: Int): DynProConfig[Decision] =
   setConfig(clazz, mode, solRange, bcMailSize, 0)

  protected final def setConfig(clazz: ConClass, mode: ConMode, solRange: Int, _bcMailSize: Int, _mxRange: Int)
  : DynProConfig[Decision] = {
    val (mxRange, bcMailSize) =
      (if(_mxRange < minRange || m - _mxRange < minRange) m else _mxRange, //m =: the matrix width
       clazz match{
         case LEFT_UP => abs(_bcMailSize)
         case UP => 1 //for now.
         case _ => 0
       })

    new DynProConfig[Decision](clazz, mode, solRange, bcMailSize, mxRange)
  }


  //default setting =: sequential mode, OVERRIDE IF NECESSARY
  //protected val config: DynProConfig[Decision] = setConfig(LEFT_UP, EVENT)

  protected val config: DynProConfig[Decision] = null //setConfig(NO_CON, __)

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
  //this block solely concerns the "matrix" method
  //private val inf = Double.NegativeInfinity
  private val hiddenMatrix: Array[Array[Option[Double]]] = Array.ofDim(n, m)
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
  protected lazy val matrix: Array[Array[Option[Double]]] = {
    /* Iterate through all the cells. Note that the order depends on the
    problem. Many versions go from top to bottom and left to right.
    However, any other order may also work. */

    def calcCellCost = for (k <- 0 until cellsSize){
      val idx = getCellIndex(k)
      //(idx) => Unit =: def handleNullState(idx: Idx){}//do nothing
      this.calcCellCost(idx, getAccValues(idx,(idx) => Unit))
    }

    val mxStart = System.nanoTime
    config match {
      case null => calcCellCost
      case _ => config.clazz match {
        case NO_DEP => calcCellCost

        case _ => config.evaluateMatrix(n, m, getAccValues, this.calcCellCost) //LEFT_UP | UP
      }
    }
    timeMap += (MATRIX -> ((System.nanoTime - mxStart) / nanoToSecFact))

    hiddenMatrix
  }


  //matlab matrix block
  private val hiddenMatlabMatrix = Array.ofDim[Double](n, m)
  private def convert(idx: Idx){
    hiddenMatlabMatrix(idx.i)(idx.j) = matrix(idx.i)(idx.j) match {
      case value: Some[Double] => value.get
      case _ => 0.0
    }
  }
  /**
   * TODO Q&D hack for matlab
   * Concerning the concurrency this stage has a very low priority.
   * Imporant: lazy, otherwise we get null pointer exceptions.
   */
  lazy val matlabMatrix: Array[Array[Double]] = {
    val start = System.nanoTime
    config match {
      case null => for (i <- 0 until n; j <- 0 until m) convert(Idx(i,j))
      case _ => config.convertMatrix(n, m, convert)
    }

    timeMap += (MATLABMX -> ((System.nanoTime - start) / nanoToSecFact))
    hiddenMatlabMatrix
  }


  /**
   * STAGE iv of iv
   * Trace back the path starting from cell (i, j)
   * @param idx
   * @return List of PathEntry
   */
  def solution(idx: Idx): List[PathEntry[Decision]] = {
    val start = System.nanoTime
    val sol = (config match {
      case null => getPath(idx, (_,_) => false)
      //(_, _) => false =: (a:Idx, b:Idx) => false =: def break(startIdx: Idx, cIdx: Idx) = false

      case _ =>
        if(config.solRange < minRange || idx.MAX - config.solRange < minRange)
        //seemly concurrent, in reality sequential when the range hasn't been set adequately
          getPath(idx, (_,_) => false)

        else config.calculateSolution(idx, getPath)
    }).toList

    val time = System.nanoTime - start
    timeMap += SOLUTION -> (time / nanoToSecFact - timeMap(MATRIX))
    timeMap += TOTAL -> time / nanoToSecFact

    if (matrixForwardPathBackward) sol.reverse else sol

  }

  /********************************************************
  Main methods (potentially concurrent) - END
  ********************************************************/


  /********************************************************
  Support methods (sequential & concurrent support) - START
    ********************************************************/
  /**
   * 06.09.2013
   * This is the recursive equivalent of the "getPath" method.
   * And as from today the OLD version of the "getPath" method.
   * Due to stack overflow issues during the computation of long sequences
   * (staring around a sequence length of 1000) i was forced to remodel it.
   * The new "getPath" method has an iterative behavior unlike the recursive one of the old method.
   * @param idx
   * @param break
   * @return
   */
  private def _getPath(idx: Idx, break:(Idx, Idx) => Boolean): ListBuffer[PathEntry[Decision]] = {
    val (eps, pathList) = (0.001, new ListBuffer[PathEntry[Decision]]())
    /*
    * Inner function.
    * Note: Requires stack size of -Xss10m
    */
    def calcSI(innerIdx: Idx) {
      // Counter for all possible solutions (might be more than 1):

      var solutionFound = false //it makes a lil bit more sense this way
      for (u <- decisions(innerIdx); if !solutionFound) {
        val prevIdx = prevStates(innerIdx, u)
        // v is difference of the current value and the previous values
        // when decision u was made:
        val v = prevIdx match {
          // Empty array:
          case Array() => calcFBack(matrix(innerIdx.i)(innerIdx.j).get, initValues, calcG)
          // Non-empty array:
          case indices: Array[Idx] => {
            val args: Array[Double] = for (pidx <- indices) yield {
              matrix(pidx.i)(pidx.j) match {
                /*
               case None => throw new RuntimeException("Internal error: " +
                       "Illegal previous state " + pidx + " at " + idx.toString)
                */
                case Some(value) => value
                case _ => initValues(0)
              }
            }
            calcFBack(matrix(innerIdx.i)(innerIdx.j).get, args, calcG)
          }
        }
        // If v (the difference) is the current value then
        // this decision was made. Note: We may get rounding errors,
        // so we use an epsilon. Also, if we have already found a
        // solution we skip any further solutions:
        if (abs(v - value(innerIdx, u)) < eps) {
          pathList += PathEntry(u, matrix(innerIdx.i)(innerIdx.j).get, innerIdx, prevIdx)
          solutionFound = true
          for (nidx <- prevIdx if !break(idx, innerIdx)) calcSI(nidx)
        }
      }
    }

    calcSI(idx)
    pathList
  }

  /**
   * This method builds the solution path within the given matrix,
   * starting from the given idx
   * @param idx cell within the given matrix
   * @param break limit (end of the solution path)
   * @return
   */
  private def getPath(idx: Idx, break:(Idx, Idx) => Boolean): ListBuffer[PathEntry[Decision]] = {
    val (eps, pathList) = (0.001, new ListBuffer[PathEntry[Decision]]())
    var _prevIdx = Array(idx)

    /*
    * Inner function.
    * Note: Unlike its recursive equivalent it doesn't require a stack size of -Xss10m
    *
    * This methods sole purpose is to enable a clean break out of the inner loop
    * since the "break" method/statement doesn't exist in scala.
    */
    def runInnerLoops {
      for(innerIdx <- _prevIdx  if !break(idx, innerIdx)){ //outer for each loop
        for (u <- decisions(innerIdx)) { //inner for each loop
          val prevIdx = prevStates(innerIdx, u)
          // v is difference of the current value and the previous values
          // when decision u was made:
          val v = prevIdx match {
            // Empty array:
            case Array() => calcFBack(matrix(innerIdx.i)(innerIdx.j).get, initValues, calcG)
            // Non-empty array:
            case indices: Array[Idx] => {
              val args: Array[Double] = for (pidx <- indices) yield {
                matrix(pidx.i)(pidx.j) match {
                  /*
                 case None => throw new RuntimeException("Internal error: " +
                         "Illegal previous state " + pidx + " at " + idx.toString)
                  */
                  case Some(value) => value
                  case _ => initValues(0)
                }
              }
              calcFBack(matrix(innerIdx.i)(innerIdx.j).get, args, calcG)
            }
          }
          // If v (the difference) is the current value then
          // this decision was made. Note: We may get rounding errors,
          // so we use an epsilon. Also, if we have already found a
          // solution we skip any further solutions:
          if (abs(v - value(innerIdx, u)) < eps) {
            pathList += PathEntry(u, matrix(innerIdx.i)(innerIdx.j).get, innerIdx, prevIdx)
            _prevIdx = prevIdx
            return //to break out of the loops
          }
        }
      }
    }

    var keepOuterForEachAlive = true
    while(keepOuterForEachAlive){
      val controlIdx = _prevIdx
      runInnerLoops

      // The path finding comes to an end ,
      // if the for each loops haven't be terminated because one acceptable solution was found,
      // or if the "break" method returns the value "true".
      if(_prevIdx == controlIdx) keepOuterForEachAlive = false
    }

      //if the _prevId is still equal to the controlIdx after the inner for each loop ist means that
      //the path reconstruction is done

    pathList
  }


  /**
   * This method is used to get all the possible cost of the given cell
   * @param idx the coordinates of the cell whose cost is about to be calculated.
   * @param handleNullState sort of protocol to follow in case a "None" value occurs in
   *                        concurrent mode.
   * @return Array[Double]
   */
  private def getAccValues(idx: Idx, handleNullState:(Idx) => Unit): Array[Double] =
  for (u <- decisions(idx)) yield prevStates(idx, u) match {
      // Empty array, i.e. there are no prev. states:
      case Array() => calcF(value(idx, u), initValues, calcG)//sum of current and previous values
      // Non-empty array:
      case prevIndexes: Array[Idx] => {
        val prevValues: Array[Double] = for (pidx <- prevIndexes) yield {
          /* Status (july 2013): This state is now known as the "null" state*/
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
      /* A double value, while overriding the initValues method, the dev should keep in mind that
       * the extreme value comes first
       */
      case Array() => initValues(0)

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
