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

import net.gumbix.dynpro.MatrixPrinter
import net.gumbix.layout.Element._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait MultipleAlignmentPrinter {
  protected val alignments: Array[Array[Alignment]]

  val strings: Array[String]

  def consensus: String
  
  protected val multipleAlignment: Array[AlignedString]

  /**
   * Format the multiple alignment.
   */
  def mkString() = {
    var element = line(multipleAlignment(0).toString)
    for (t <- 1 until multipleAlignment.size) {
      val btw = for (i <- 0 until multipleAlignment(t).size) yield {
        if (multipleAlignment(t - 1).isGapAt(i) || multipleAlignment(t).isGapAt(i)) " "
        else if (multipleAlignment(t - 1)(i) == multipleAlignment(t)(i)) "|"
        else " "
      }
      element = element above line(btw.mkString) above line(multipleAlignment(t).toString)
    }
    element = element above line("-" * consensus.size) above line(consensus)
    element.toString
  }

  def _mkAlignmentTable() {
    object o extends MatrixPrinter[Double] {
      val getMatrix = {
        val n = strings.size
        val matrix: Array[Array[Option[Double]]] = Array.ofDim(n, n)
        for (i <- 0 until n; j <- 0 until n) {
          matrix(i)(j) = Some(alignments(i)(j).similarity)
        }
        matrix
      }

      override def columnLabels = None
    }
    o.mkMatrixString
  }
}