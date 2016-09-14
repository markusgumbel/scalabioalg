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
package net.gumbix.util

import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import net.gumbix.layout.Element._

/**
  * Create a string of a formatted matrix. Matrix is of type Option[Double], i.e.
  * there can be cells with no value (=None).
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait MatrixPrinter {

  class Formatter(val f: DecimalFormat) {
    f.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.US))

    def format(n: Double) = f.format(n)
  }

  val INT = new Formatter(new DecimalFormat("0"))
  val DECIMAL = new Formatter(new DecimalFormat("0.00"))
  val ENGINEER = new Formatter(new DecimalFormat("0.000E00"))

  /**
    * Specifies how the numeric values are formatted.
    */
  var formatter = ENGINEER

  def matrix: Array[Array[Option[Double]]]

  var innerColumnSeparator = ' '

  /**
    * What labels should the columns get?
    */
  def columnLabels: Option[Array[String]] = {
    Some((1 to matrix(0).length).toList.map("col" + _.toString).toArray)
  }

  /**
    * What labels should the rows get?
    */
  def rowLabels: Array[String] = {
    (1 to matrix.length).toList.map("row" + _.toString).toArray
  }

  /**
    * A counter for each column, beginning with 0.
    */
  def columnCounter =
    (0 until matrix(0).length).map(_.toString).toArray

  /**
    * A counter for each row, beginning with 0.
    */
  def rowCounter =
    (0 until matrix.length).map(_.toString).toArray

  /**
    * Create a string that represents the entire matrix.
    */
  def mkMatrixString = {
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
        col = col above line(o)
      }
      table = table beside col beside
        expandableLine(innerColumnSeparator.toString, innerColumnSeparator)
    }
    table.toString
  }

  def mkMatrixLaTeXString(): String = {
    mkMatrixLaTeXString(() => "")
  }

  def mkMatrixLaTeXString(latexPostProcessing: () => String): String = {
    def seq2LaTeXLine(columns: List[String]) = {
      columns.mkString("  ", " \\& ", " \\\\\n")
    }

    var s = "\\begin{tikzpicture}\n" +
      "\\tikzstyle{label}=[font={\\scriptsize}]\n" +
      // Style alignment is defined in BIM\bim_preamble.tex
      "\\matrix (matrix) [matrix of nodes,nodes={draw=black,minimum width=1cm}," +
      "text depth=.25ex,text height=1.5ex,nodes in empty cells," +
      "row sep=-\\pgflinewidth,column sep=-\\pgflinewidth," +
      "ampersand replacement=\\&] {\n"
    // ,alignment

    for (lines <- matrix) {
      val values = lines.toList.map {
        c =>
          c match {
            case None => " "
            case Some(a) => formatter.format(a)
          }
      }
      s += seq2LaTeXLine(values)
    }

    s += "};\n"
    // Columns:
    if (columnLabels != None) {
      for (j <- (0 until columnCounter.size)) {
        val label = "matrix-1-" + (j + 1)
        s += "\\node[above=2mm of " + label + ",label] {\\scriptsize " + columnLabels.get(j) + "};\n"
      }
    }
    // Rows:
    for (i <- (0 until rowCounter.size)) {
      val label = "matrix-" + (i + 1) + "-1"
      s += "\\node[left=2mm of " + label + ",label] {" + rowLabels(i) + "};\n"
    }
    s += latexPostProcessing() + "\n"
    s += "\\end{tikzpicture}"
    s
  }
}