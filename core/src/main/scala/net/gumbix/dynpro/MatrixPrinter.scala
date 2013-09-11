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

import scala.actors.Actor

import java.util.Locale
import java.text.{DecimalFormatSymbols, DecimalFormat}

import net.gumbix.layout.Element
import Element._
import concurrency.Messages.START

/**
 * Create a string of a formatted matrix. Matrix is of type Option[Double], i.e.
 * there can be cells with no value (=None).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait MatrixPrinter[Decision] {
  class Formatter(val f: DecimalFormat) {
    f.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.US))

    def format(n: Double) = f.format(n)
  }

  val INT = new Formatter(new DecimalFormat("0."))
  val DECIMAL = new Formatter(new DecimalFormat("0.00"))
  val ENGINEER = new Formatter(new DecimalFormat("0.000E00"))
  var formatter = ENGINEER //Specifies how the numeric values are formatted.
  var innerColumnSeparator = ' '

  protected val matrix: Array[Array[Option[Double]]]

  /**
   * What labels should the columns get?
   */
  protected def columnLabels: Option[Array[String]] = {
    Some((1 to matrix(0).length).toList.map("col" + _.toString).toArray)
  }

  /**
   * What labels should the rows get?
   */
  protected def rowLabels: Array[String] = {
    (1 to matrix.length).toList.map("row" + _.toString).toArray
  }

  /**
   * A counter for each column, beginning with 0.
   */
  private def columnCounter =
    (0 until matrix(0).length).map(_.toString).toArray

  /**
   * A counter for each row, beginning with 0.
   */
  private def rowCounter =
    (0 until matrix.length).map(_.toString).toArray

  /**
   *
   * @return
   */
  private def getColumns: (Element, Element) = {
    align = net.gumbix.layout.Element.CENTER //align is used in the "line" and "expandableLine" methods

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

    (firstColumn, secondColumn)
  }


  /***** CONCURRENT CREATION - START *****/
  /**
   *
   * @param tbActor
   * @param j
   * @param pathMatrix
   */
  private class ColumnActor(tbActor: TableActor, j: Int, pathMatrix: Array[Array[Boolean]]) extends Actor{
    align = net.gumbix.layout.Element.CENTER
    private var col = (columnLabels match {
      case None => line(columnCounter(j))
      case Some(labels) => line(columnCounter(j)) above line(labels(j))
    }) above expandableLine("-", '-')

    def act{
      align = net.gumbix.layout.Element.NUMBER
      for (i <- 0 until matrix.size) {
        val o = matrix(i)(j) match {
          case None => "_"
          case Some(value) => formatter.format(value)
        }
        col = col above line(if(pathMatrix(i)(j)) o+"*" else o)
      }

      tbActor ! (j, col)
    }
  }

  /**
   *
   * @param pathMatrix
   */
  private class TableActor(pathMatrix: Array[Array[Boolean]]) extends Actor{
    private val _columns = getColumns
    private var table = _columns._1 beside expandableLine(" ", ' ') beside _columns._2 beside expandableLine("|", '|')

    def act{react{case START =>
      //Attributes
      val (columns, loopEnd) = (scala.collection.mutable.Map[Int, Element](), matrix(0).size)
      lazy val andThenBlock = { //instructions to be procee after the loopWhile below
        val sortedKeys = columns.keys.toList.sorted
        sortedKeys.foreach{j =>
          table = table beside columns(j) beside expandableLine(innerColumnSeparator.toString, innerColumnSeparator)
        }
        reply(table) //return Element
      }

      //Methods
      for (j <- 0 until loopEnd) new ColumnActor(this, j, pathMatrix).start
      loopWhile(columns.size < loopEnd){
        react{
          case (j:Int, col: Element) => columns(j) = col
        }
      }andThen andThenBlock
    }}
  }


  /**
   *
   * @param pathMatrix
   * @return
   */
  private def _makeTable(pathMatrix: Array[Array[Boolean]]): Element = {
    val actor = new TableActor(pathMatrix)
    actor.start
    actor !? START match{case table: Element => table} //return
  }

  /**
   * Create a string that represents the entire matrix.
   */
  def _mkMatrixString = _makeTable(Array.ofDim[Boolean](matrix.size, matrix(0).size)).toString

  /**
   * Create a string that represents the entire matrix.
   * @param solution see <code>mkMatrixString</code>
   */
  def _mkMatrixString(solution: List[PathEntry[Decision]]) = {
    val pathMatrix: Array[Array[Boolean]] = Array.ofDim(matrix.size, matrix(0).size)
    /* 11.09.2013
     * There's no need for this loop. The compiler's Boolean default value is false
     * for (i <- 0 until matrix.size; j <- 0 until matrix(0).size) pathMatrix(i)(j) = false
     */
    solution.foreach(x => pathMatrix(x.currCell.i)(x.currCell.j) = true)
    _makeTable(pathMatrix).toString
  }
  /***** CONCURRENT CREATION - END *****/


  /***** SEQUENTIAL CREATION - START *****/
  private def makeTable(pathMatrix: Array[Array[Boolean]]): Element = {
    val columns = getColumns
    var table = columns._1 beside expandableLine(" ", ' ') beside columns._2 beside expandableLine("|", '|')

    for (j <- 0 until matrix(0).size) {

      align = net.gumbix.layout.Element.CENTER
      var col = (columnLabels match {
        case None => line(columnCounter(j))
        case Some(labels) => line(columnCounter(j)) above line(labels(j))
      }) above expandableLine("-", '-')

      align = net.gumbix.layout.Element.NUMBER
      for (i <- 0 until matrix.size) {
        val o = matrix(i)(j) match {
          case None => "_"
          case Some(value) => formatter.format(value)
        }
        col = col above line(if(pathMatrix(i)(j)) o+"*" else o)
      }

      table = table beside col beside expandableLine(innerColumnSeparator.toString, innerColumnSeparator)
    }
    table
  }

  /**
   * Create a string that represents the entire matrix.
   */
  def mkMatrixString = makeTable(Array.ofDim[Boolean](matrix.size, matrix(0).size)).toString

  /**
   * Create a string that represents the entire matrix.
   * @param solution A path through the matrix that represents a
   * solution in terms of dynamic programming. An asterisk is added to
   * each cell of the solution path.
   */
  def mkMatrixString(solution: List[PathEntry[Decision]]) = {
    val pathMatrix: Array[Array[Boolean]] = Array.ofDim(matrix.size, matrix(0).size)
    /* 11.09.2013
     * There's no need for this loop. The compiler's Boolean default value is false
     * for (i <- 0 until matrix.size; j <- 0 until matrix(0).size) pathMatrix(i)(j) = false
     */
    solution.foreach(x => pathMatrix(x.currCell.i)(x.currCell.j) = true)
    makeTable(pathMatrix).toString
  }
  /***** SEQUENTIAL CREATION - END *****/

}
