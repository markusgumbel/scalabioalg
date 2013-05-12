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
import net.gumbix.dynpro.concurrency
import concurrency.{Concurrency}
import net.gumbix.dynpro.concurrency.DependencyCase._
import net.gumbix.dynpro.concurrency.ConcurrencyMode._
import scala.Some

/**
 * Convenient class for a two-dimensional index pair (i, j).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
case class Idx(i: Int, j: Int) {
  def +(o: Idx) = Idx(i + o.i, j + o.j)
  override def toString = "(" + i + ", " + j + ")"

  //required to simplify the concurrency - start
  def +(z: Int) = Idx(i + z, j + z)
  def +(zi: Int, zj: Int) = Idx(i + zi, j + zj)
  def -(z: Int) = Idx(i - z, j - z)
  def -(zi: Int, zj: Int) = Idx(i - zi, j - zj)

  def ==(o: Idx) = (i == o.i && j == o.j)
  //required to simplify the concurrency - end

}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results. 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
abstract class DynPro[Decision] extends DynProBasic{

  /********************************************************
  Abstract attributes and methods - START
  ********************************************************/
  //default configuration, OVERRIDE IF NECESSARY
  val con = new Concurrency[Decision](NOT_CONCURRENT)

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
  Abstract attributes and methods - END
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

    /* create an empty n*m matrix. Value{i,j} =: None
    n = mx.length
    m = mx(0).length */
    var mx = con.dep match {
      case NOT_CONCURRENT =>
        val _mx: Array[Array[Option[Double]]] = Array.ofDim(n, m)
        for (i <- 0 until n; j <- 0 until m) {
          _mx(i)(j) = None
        }
        _mx
      case _ => con.emptyMatrix(n, m)
    }

    //CAUTION: Do not merge both blocks

    //evaluate the matrix cells sequentially or concurrently
    con.dep match {
      case LEFT_UPLEFT_UP_UPRIGHT =>
        con.computeMatrix(mx, initValues(0), n, m, calcMatrixIndexValue)

      case UPLEFT_UP_UPRIGHT =>
        con.computeMatrix(mx, initValues(0), m, n, calcMatrixIndexValue)

      case _ => for (k <- 0 until cellsSize)
          mx = calcMatrixIndexValue(mx, getCellIndex(k), parsePrevValues, handleNewValue)

        mx
    }
  }


  /**
   * TODO Q&D hack for matlab
   * Imporant: lazy, otherwise we get null pointer exceptions.
   */
  lazy val matlabMatrix: Array[Array[Double]] = con.dep match {
      case NOT_CONCURRENT =>
        val mx: Array[Array[Double]] = Array.ofDim(n, m)
        for (i <- 0 until n; j <- 0 until m) {
          mx(i)(j) = matrix(i)(j) match {
            case a: Some[Double] => a.get
            case _ => 0
          }
        }
        mx

      case _ => con.convertMatrix(matrix)
    }


  /**
   * Trace back the path starting from cell (i, j)
   * @param idx
   * @return List of PathEntry
   */
  def solution(idx: Idx): List[PathEntry[Decision]] = {
    val solution = (con.dep match {
      //(Idx) => false =: {def break(_idx: Idx) = false}
      case NOT_CONCURRENT => calculateSolution(idx, (Idx) => false)

      case _ => con.calculateSolution(idx, calculateSolution)
    }).toList

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
  private def calculateSolution(idx: Idx,  break:(Idx) => Boolean): ListBuffer[PathEntry[Decision]] = {
    val eps = 0.001
    val listBuffer = new ListBuffer[PathEntry[Decision]]()
    /*
    * Inner function.
    * Note: Requires stack size of -Xss10m
    */
    def calcSI(idx: Idx) {
      // Counter for all possible solutions (might be more than 1):
      //var count = 0
      var solutionFound = false //it makes a lil bit more sense this way
      for (u <- decisions(idx); if !solutionFound) {
        //TODO 08.05 implement the concept and merge both DynPor's
        val prevIdx = prevStates(idx, u)
        // v is difference of the current value and the previous values
        // when decision u was made:
        val v = prevIdx match {
          // Empty array:
          case Array() => calcFBack(matrix(idx.i)(idx.j).get, initValues, calcG)
          // Non-empty array:
          case indices: Array[Idx] => {
            val args: Array[Double] = for (pidx <- indices) yield {
              matrix(pidx.i)(pidx.j) match {
                /*
               case None => throw new RuntimeException("Internal error: " +
                       "Illegal previous state " + pidx + " at " + idx.toString)
                */
                case None => initValues(0)
                case Some(value) => value
              }
            }
            calcFBack(matrix(idx.i)(idx.j).get, args, calcG)
          }
        }
        // If v (the difference) is the current value then
        // this decision was made. Note: We may get rounding errors,
        // so we use an epsilon. Also, if we have already found a
        // solution we skip any further solutions:
        if (abs(v - value(idx, u)) < eps) {
          listBuffer += new PathEntry(u, matrix(idx.i)(idx.j).get, idx, prevIdx)
          //count += 1
          solutionFound = true
          for (nidx <- prevIdx if !break(idx)) calcSI(nidx)
        }
      }
    }

    calcSI(idx)
    listBuffer
  }



  private def parsePrevValues(prevIndexes: Array[Idx], prevValues: Array[Double]) = prevValues
  private def handleNewValue(idx: Idx, newValue: Double){} //do nothing


  /**
   *
   * @param currentMX
   * @param idx
   * @param parsePrevValues
   * @param handleNewValue
   * @return
   */
  private def calcMatrixIndexValue(currentMX: Array[Array[Option[Double]]],
                                             idx: Idx,
                                             parsePrevValues:(Array[Idx], Array[Double]) => Array[Double],
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
          calcF(value(idx, u), parsePrevValues(prevIndexes, prevValues), calcG)
          //calcF(value(idx, u), prevValues, calcG)
        }
      }
    }


    // Calculate the new value (min. or max.):
    val newValue: Double = values match {
      case Array() => initValues(0)
      /*a double value, while overriding the initValues method, the dev should keep in mind that
      * the extreme value comes first*/

      case _ => values.toList.reduceLeft(extremeFunction(_, _)) //the maximum or min
    }

    handleNewValue(idx, newValue)

    currentMX(idx.i)(idx.j) = Some(reviseMax(newValue))
    currentMX //return
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
