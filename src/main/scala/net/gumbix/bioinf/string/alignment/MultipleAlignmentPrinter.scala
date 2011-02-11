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

  def mkAlignmentTable() {
    object o extends MatrixPrinter[Double] {
      val matrix = {
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