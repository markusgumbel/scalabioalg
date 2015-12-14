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

/**
 * Convenient class for a two-dimensional index pair (i, j).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
case class Idx(i: Int, j: Int) {
  def +(o: Idx) = Idx(i + o.i, j + o.j)

  override def toString = "(" + i + ", " + j + ")"
}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results. 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
abstract class DynPro[Decision] extends DynProMatrix {

  /**
   * TODO Appears to be quick and dirty.
   * By default, first the matrix is created by going forward, i.e.
   * starting from (0, 0) going to (n-1, m-1). Then the decision path
   * is calculated by going backwards to cell (0, 0).
   */
  def matrixForwardPathBackward = true

  /**
   * How many cells need to be computed?
   */
  def cellsSize = n * m

  /**
   * Get the cell index for the k-th step where k is in range
   * 0 to cellSize - 1. The default order is to iterate over the matrix
   * from left to right beginning with the first row, i.e.
   * (i, j) = (k / m, k mod m).
   * TODO replace with iterator?
   * @param k is in range 0 to cellsSize-1.
   */
  def getCellIndex(k: Int) = {
    val row = k / m
    val col = k % m
    Idx(row, col)
  }

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
  def initValues: Array[Double] = Array(0)

  val MIN = (x1: Double, x2: Double) => if (x1 < x2) x1 else x2
  val MAX = (x1: Double, x2: Double) => if (x1 > x2) x1 else x2

  /**
   * Set this function to specify whether the algorithm
   * calculates a minimum or a maximum. Default is maximum.
   */
  def extremeFunction = MAX

  /**
   * TODO Both variants.
   *
   * The recursive expression to calculate the accumulated values (costs)
   * when the current value (in state s_i) and the previous accumulated
   * values (of state s_      { i-1 } ) are given.
   * Note that the decision is not needed as we only define the function here.  
   * Default is: accValue = currentValue + g(prevsAccValues)
   * @param currentValue The value for the current state (given a decision)
   * @param prevsAccValues Array of previous values (given a decision).
   * Most of the dynamic optimization problems have exactly one previous value.
   * @param g Function that calculates the previous values.
   */
  def calcF(currentValue: Double, prevsAccValues: Array[Double], g: (Array[Double]) => Double) =
    currentValue + g(prevsAccValues)

  def calcFBack(current: Double, prevs: Array[Double], g: (Array[Double]) => Double) =
    current - g(prevs)

  /**
   * The function g specifies how the accumulated values are calculated
   * if the dyn. opt. has more than one previous states and hence more than
   * one previous accumulated values.
   * Default: g(x) = x1 + x2 + ... x_n
   */
  def calcG(x: Array[Double]) = if (x.isEmpty) 0 else x.reduceLeft(_ + _)

  /**
   * Local alignment needs to reset an accumulated value to 0.
   * Default is: reviseMax(max) := max.
   */
  def reviseMax(max: Double): Double = max

  /**
   * matrix containing the accumulated values (costs).
   */
  lazy val matrix: Array[Array[Option[Double]]] = {
    val mx: Array[Array[Option[Double]]] = Array.ofDim(n, m)
    for (i <- 0 until n; j <- 0 until m) {
      mx(i)(j) = None
    }
    // Iterate through all the cells. Note that the order depends on the
    // problem. Many versions go from top to bottom and left to right.
    // However, any other order may also work.
    for (k <- 0 until cellsSize) {
      val idx = getCellIndex(k)
      // Get a list of values (or empty list):
      val values = for (u <- decisions(idx)) yield {
        prevStates(idx, u) match {
        // Empty array, i.e. there are no prev. states:
          case Array() => calcF(value(idx, u), initValues, calcG)
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
            calcF(value(idx, u), args, calcG)
          }
        }
      }
      // Calculate the extreme value (min. or max.):
      val extrValue: Double = values match {
      // TODO why index position 0?
        case Array() => initValues(0)
        case _ => values.toList.reduceLeft(extremeFunction(_, _))
      }
      mx(idx.i)(idx.j) = Some(reviseMax(extrValue))
    }
    mx
  }

  /**
   * TODO Q&D hack for matlab
   * Imporant: lazy, otherwise we get null pointer exceptions.
   */
  lazy val matlabMatrix: Array[Array[Double]] = {
    val mx: Array[Array[Double]] = Array.ofDim(n, m)
    for (i <- 0 until n; j <- 0 until m) {
      mx(i)(j) = matrix(i)(j) match {
        case a: Some[Double] => a.get
        case _ => 0
      }
    }
    mx
  }

  /**
   * Trace back the path starting from cell (i, j)
   * @param idx
   * @return List of PathEntry
   */
  def solution(idx: Idx): List[PathEntry[Decision]] = {

    val listBuffer = new ListBuffer[PathEntry[Decision]]()

    /*
    * Inner function.
    * Note: Requires stack size of -Xss10m
    */
    def calculateSolutionInternal(idx: Idx) {
      // Counter for all possible solutions (might be more than 1):
      var count = 0
      for (u <- decisions(idx); if count == 0) {
        val prevIdx = prevStates(idx, u)
        // h is difference of the current value and the previous values
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
          count += 1
          for (nidx <- prevIdx) {
            calculateSolutionInternal(nidx)
          }
        }
      }
    }

    calculateSolutionInternal(idx)
    if (matrixForwardPathBackward) listBuffer.toList.reverse
    else listBuffer.toList
  }

  private def eps = 0.001
}

// because of Scala bug?!
// solved in build with timestamp 2010 10 19
class IntDynPro extends DynPro[java.lang.Integer] with MatrixPrinter[java.lang.Integer] {
  def n = 1

  def m = 1

  def decisions(idx: Idx) = Array(1)

  def prevStates(idx: Idx, d: java.lang.Integer) = Array()

  def value(idx: Idx, d: java.lang.Integer) = 0

  formatter = INT
}

abstract class DynProJava[Decision] extends DynPro[Decision] with MatrixPrinter[Decision] {
  formatter = INT
}
