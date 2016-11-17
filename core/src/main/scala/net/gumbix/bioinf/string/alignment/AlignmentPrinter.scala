package net.gumbix.bioinf.string.alignment

import net.gumbix.layout.Element._

/**
  * A base class for printing alignments to console or LaTeX/Tikz.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait AlignmentPrinter {

  /**
    * Creates an alignment for console output.
    *
    * @param als1 The first aligned string.
    * @param als2 The second aligned string.
    * @return Two strings and a line inbetween.
    */
  def makeAlignmentString(als1: AlignedString, als2: AlignedString): String = {
    val separator = for (i <- 0 until als1.size) yield {
      if (als1.isGapAt(i) || als2.isGapAt(i)) " "
      else if (als1(i) == als2(i)) "|" else " "
    }

    // TODO some kind of global variable, so we need to set the alignment
    val s = line(als1.toString, 0) above
      line(separator.mkString, 0) above
      line(als2.toString, 0)
    s.toString
  }
}
