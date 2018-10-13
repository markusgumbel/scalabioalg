package net.gumbix.bioinf.string.alignment

import net.gumbix.bioinf.string.alignment.GapType.GAP

/**
  * Some methods for progressive multiple alignment algorithms.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait ProgressiveAlignment {

  /**
    * Insert gaps. Start from the end of the sorted list
    * of positions as this avoids a recalculation of
    * the shifted indices.
    */
  def insertGaps(s: AlignedString, gaps: List[Int]) {
    gaps.reverse.foreach(s.insertGapBefore(_, GAP))
  }
}
