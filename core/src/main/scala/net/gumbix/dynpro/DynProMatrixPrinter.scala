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

import net.gumbix.layout.Element._
import java.util.Locale
import java.text.{DecimalFormatSymbols, DecimalFormat}

import net.gumbix.util.MatrixPrinter

/**
 * Create a string of a formatted matrix. Matrix is of type Option[Double], i.e.
 * there can be cells with no value (=None).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait DynProMatrixPrinter[Decision] extends MatrixPrinter {

  /**
   * Create a string that represents the entire matrix.
   */
  override def mkMatrixString =
    makeTable(matrix, Array.ofDim(matrix.size, matrix(0).size)).toString

  /**
   * Create a string that represents the entire matrix.
   * @param solution A path through the matrix that represents a
   * solution in terms of dynamic programming. An asterisk is added to
   * each cell of the solution path.
   */
  def mkMatrixString(solution: List[PathEntry[Decision]]) = {

    val pathMatrix: Array[Array[Boolean]] = Array.ofDim(matrix.size, matrix(0).size)
    for (i <- 0 until matrix.size; j <- 0 until matrix(0).size) {
      pathMatrix(i)(j) = false
    }
    solution.foreach(x => pathMatrix(x.currCell.i)(x.currCell.j) = true)
    makeTable(matrix, pathMatrix).toString
  }

  def makeTable(matrix: Array[Array[Option[Double]]], pathMatrix: Array[Array[Boolean]]):
  net.gumbix.layout.Element = {

    align = net.gumbix.layout.Element.CENTER

    var firstColumn = if (columnLabels != None) {
      line("") above line("") above expandableLine("-", '-')
    } else {
      line("") above expandableLine("-", '-')
    }
    rowCounter.foreach(r => firstColumn = firstColumn above line(r))

    var secondColumn = if (columnLabels != None) {
      line("") above line("") above expandableLine("-", '-')
    } else {
      line("") above expandableLine("-", '-')
    }

    rowLabels.foreach(r => secondColumn = secondColumn above line(r))

    var table = firstColumn beside
            expandableLine(" ", ' ') beside secondColumn beside expandableLine("|", '|')
    for (j <- 0 until matrix(0).size) {
      align = net.gumbix.layout.Element.CENTER
      var col = columnLabels match {
        case None => line(columnCounter(j))
        case Some(labels) => line(columnCounter(j)) above line(labels(j))
      }
      col = col above expandableLine("-", '-')
      align = net.gumbix.layout.Element.NUMBER
      for (i <- 0 until matrix.size) {
        val o = matrix(i)(j) match {
          case None => "_"
          case Some(value) => formatter.format(value)
        }
        val p = if (pathMatrix(i)(j)) o + "*" else o
        col = col above line(p)
      }
      table = table beside col beside
              expandableLine(innerColumnSeparator.toString, innerColumnSeparator)
    }
    table
  }
}