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

import net.gumbix.dynpro.CellPosition._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait Backpropagation[Decision] extends DynProMatrix {
  def solution(idx: Idx): List[PathEntry[Decision]]

  /**
   * By default, start at the end of the matrix.
   */
  val backpropagationStart: CellPosition = MAXIMUM_INDICES

  /**
   * Trace back the path. Start cell depends on
   * setting of attribute <code>backpropagationStart</code>.
   */
  def solution: List[PathEntry[Decision]] = {
    val idx = cellIndices(backpropagationStart)
    solution(idx)
  }

  /**
   * @return Index (i, j) of the cell with the maximum value.
   */
  def cellIndices(how: CellPosition) = how match {
    case MAXIMUM_VALUE => maxIdx
    case MAXIMUM_VALUE_LAST_ROW => maxLastRowIdx
    case MAXIMUM_VALUE_LAST_COLUMN => maxLastColumnIdx
    case MAXIMUM_INDICES => Idx(n - 1, m - 1)
    case _ => throw new RuntimeException("Illegal position.")
  }


  //TODO concurrently look for the max index
  lazy val maxIdx = {
    var max = java.lang.Double.NEGATIVE_INFINITY
    var maxIdx = Idx(0, 0)
    for (i <- 0 until n; j <- 0 until m) {
      matrix(i)(j) match {
        case Some(value) => {
          if (value > max) {
            max = value
            maxIdx = Idx(i, j)
          }
        }
        case _ =>
      }
    }
    maxIdx
  }

  /**
   * @return The cell indices of the maximum value in the last row.
   */
  lazy val maxLastRowIdx = {
    val row = for (e <- matrix(n - 1) if e.isInstanceOf[Some[Double]]) yield e.get
    val max = row reduceLeft (_ max _)
    val colIdx = row.indexOf(max)
    Idx(n - 1, colIdx)
  }

  /**
   * @return The cell indices of the maximum value in the last column.
   */
  lazy val maxLastColumnIdx = {
    val columnOptional = for (i <- 0 until n) yield matrix(i)(m - 1)
    val column = for (e <- columnOptional if e.isInstanceOf[Some[Double]]) yield e.get
    val max = column reduceLeft (_ max _)
    val rowIdx = column.indexOf(max)
    Idx(rowIdx, m - 1)
  }
}
