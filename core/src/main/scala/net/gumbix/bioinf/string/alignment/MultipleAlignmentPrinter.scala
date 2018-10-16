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
package net.gumbix.bioinf.string.alignment

import net.gumbix.layout.Element._
import net.gumbix.util.MatrixPrinter

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait MultipleAlignmentPrinter {
  val alignments: Array[Array[Alignment]]

  val strings: Array[String]

  def consensus: String

  val multipleAlignment: Array[AlignedString]

  /**
    * Format the multiple alignment.
    */
  private def mkString(msa: Array[AlignedString]) = {
    var element = line(msa(0).toString)
    for (t <- 1 until msa.size) {
      val btw = for (i <- 0 until msa(t).size) yield {
        if (msa(t - 1).isGapAt(i) || msa(t).isGapAt(i)) " "
        else if (msa(t - 1)(i) == msa(t)(i)) "|"
        else " "
      }
      element = element above line(btw.mkString) above line(msa(t).toString)
    }
    element = element above line("-" * consensus.size) above line(consensus)
    element.toString
  }

  def mkString(): String = mkString(true)

  def mkString(sort: Boolean): String = {
    if (sort) {
      val smsa = multipleAlignment.sortWith(_.alignedString < _.alignedString)
      mkString(smsa)
    } else {
      mkString(multipleAlignment)
    }
  }

  def mkAlignmentTable() = {
    object o extends MatrixPrinter {
      val matrix = {
        val n = strings.size
        val matrix: Array[Array[Option[Double]]] = Array.ofDim(n, n)
        for (i <- 0 until n; j <- 0 until n) {
          matrix(i)(j) = Some(alignments(i)(j).similarity)
        }
        matrix
      }

      override def columnLabels = None
      override def rowLabels = None
    }
    o.mkMatrixString
  }
}