package net.gumbix.bioinf.string.alignment

import net.gumbix.layout.Element._
import net.gumbix.dynpro.PathEntry

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait AlignmentPrinter[Decision] {

  def alignedStrings(solution: List[PathEntry[Decision]]):
  Tuple2[AlignedString, AlignedString]

  /**
   * Creates an alignment.
   * @param solution A solution for the string alignment.
   * @return Two strings and a line inbetween.
   */
  def makeAlignmentString(solution: List[PathEntry[Decision]]) = {
    val (als1, als2) = alignedStrings(solution)

    val separator = for (i <- 0 until als1.size) yield {
      if (als1.isGapAt(i) || als2.isGapAt(i)) " "
      else if (als1(i) == als2(i)) "|" else " "
    }

    // TODO some kind of global variable, so we need to set the alignment
    line(als1.toString, 0) above
            line(separator.mkString, 0) above
            line(als2.toString, 0)
  }
}