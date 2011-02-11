package net.gumbix.bioinf.string.alignment

/**
 * s1: -GCT
 * s2: A-CA
 *     IDMS
 * is supposed to be the search string whereas s2 is the (longer) original
 * string. So it should be |s1| < |s2|.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object AlignmentStep extends Enumeration {
  type AlignmentStep = Value

  /**
   * Proceed one step on string 1,
   * <==> a character on s1 needs to be deleted to align with s2.
   */
  val DELETE = Value("D")

  /**
   * Proceed one step on string 2,
   * <==> a character on s1 needs to be inserted to align with s2.
   */
  val INSERT = Value("I")

  val BOTH = Value("B")
  val MATCH = Value("M")
  val SUBSTITUTION = Value("S")

  /**
   * Initial decision to do nothing.
   * TODO Perhaps obsolete if DP algorithms is modified.
   */
  val NOTHING = Value("N")
}

/*
* s1 is supposed to be the search string whereas s2 is the (longer) original
* string. So it should be |s1| < |s2|.
*/
object AlignmentMode extends Enumeration {
  type AlignmentMode = Value

  /**
   * Do a global alignment (Needleman/Wunsch)
   */
  val GLOBAL = Value("Global (Needleman/Wunsch)")

  /**
   * String s1 is aligned to the right. I.e. gaps at the beginning of
   * s1 are for free.
   */
  val LOCAL_RIGHT_ALIGNMENT = Value("s1 Aligned Local Right")

  /**
   * String s1 is aligned to the left. I.e. gaps at the end of
   * s1 are for free.
   */
  val LOCAL_LEFT_ALIGNMENT = Value("s1 Aligned Local Left")

  /**
   * Free gaps on s1 on both sides.
   */
  val LOCAL_CENTERED = Value("s1 Aligned Local Centered")

  /**
   * Real local alignment (Smith-Waterman).
   */
  val LOCAL_OPTIMAL = Value("Local (Smith/Waterman)")
}