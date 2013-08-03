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

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait DynProMatrix {

  /**
   * The number of rows of the matrix.
   */
  def n: Int

  /**
   * The number of columns of the matrix.
   */
  def m: Int

  /**
   * Matrix containing the accumulated values (costs).
   * A cell may be empty (=None).
   */
  val matrix: Array[Array[Option[Double]]]

}


/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 8:22 AM
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de), Patrick Meppe (tapmeppe@gmail.com)
 */
abstract class DynProBasic[Decision] extends DynProMatrix{

  /********************************************************
  Support attributes and methods (mainly sequential support) - START
    ********************************************************/

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
   * Extreme functions
   */
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
   * @param prevValues Array of previous values (given a decision).
   * Most of the dynamic optimization problems have exactly one previous value.
   * @param gDef Function that calculates the previous values.
   * @return
   */
  def calcF(currentValue: Double, prevValues: Array[Double], gDef: (Array[Double]) => Double) =
    currentValue + gDef(prevValues)


  /**
   * @see the documentation of the method above
   */
  def calcFBack(currentValue: Double, prevValues: Array[Double], gDef: (Array[Double]) => Double) =
    currentValue - gDef(prevValues)


  /**
   * The function g specifies how the accumulated values are calculated
   * if the dyn. opt. has more than one previous states and hence more than
   * one previous accumulated values.
   * Default: g(x) = x1 + x2 + ... x_n
   */
  def calcG(x: Array[Double]) = if (x.isEmpty) 0.0 else x.reduceLeft(_ + _)


  /**
   * Local alignment needs to reset an accumulated value to 0.
   * Default is: reviseMax(max) := max.
   */
  def reviseMax(max: Double): Double = max

  /********************************************************
  Support attributes and methods (mainly sequential support) - END
    ********************************************************/

}
