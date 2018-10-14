package net.gumbix.bioinf.string.alignment

import net.gumbix.bioinf.string.alignment.GapType.GAP

/**
  * Some methods for progressive multiple alignment algorithms.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait ProgressiveAlignment {

  /**
    * Calculate the positions in the strings s1 and s2 where a
    * gap needs to be inserted (before) to make the two strings
    * compatible. The principle is: once a gap, always a gap.
    * @param s1 First aligned string
    * @param s2 Second aligned string
    * @return a tuple of two lists containing the insert-positions
    * for string s1 and s2.
    */
  def getInsertions(s1: AlignedString, s2: AlignedString):
  Tuple2[List[Int], List[Int]] = {
    var i = 0
    var j = 0
    var e1: List[Int] = Nil
    var e2: List[Int] = Nil

    // Read a char c1 and c2 from the strings s1 and s2.
    // Move the pointer for c1 and c2 forward depending
    // on the read characters.
    while (i < s1.size && j < s2.size) {
      if ((s1.isGapAt(i) && s2.isGapAt(j)) || s1(i) == s2(j)) {
        // Case 1: Both characters are equal.
        // Do nothing but move both pointers:
        i += 1
        j += 1
      } else if (s1.isGapAt(i)) {
        // Case 2: c1 is a gap. A gap must be
        // inserted on s2 at position j.
        // Move only pointer c1 and leave c2 where it is.
        e2 = j :: e2
        i += 1
      } else {
        // Case 3: c2 has a gap. A gap must be
        // inserted on s1 at position i.
        // Move only pointer c2 and leave c1 where it is.
        e1 = i :: e1
        j += 1
      }
    }
    // There might be unread characters on either s1 or s2.
    (i until s1.size).foreach {
      _ => e2 = j :: e2
    }
    (j until s2.size).foreach {
      _ => e1 = i :: e1
    }
    (e1.reverse, e2.reverse)
  }

  /**
    * Insert gaps. Start from the end of the sorted list
    * of positions as this avoids a recalculation of
    * the shifted indices.
    */
  def insertGaps(s: AlignedString, gaps: List[Int]) {
    gaps.reverse.foreach(s.insertGapBefore(_, GAP))
  }
}
