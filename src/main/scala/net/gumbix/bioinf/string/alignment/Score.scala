package net.gumbix.bioinf.string.alignment

import net.gumbix.bioinf.string.alignment.AlignmentStep._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait Score {

  val substMatrix: Option[String] = None
  
  /**
   * Decision -> Value. What is the value for the decision?
   */
  val values = Map(INSERT -> -2, DELETE -> -2, MATCH -> 1, SUBSTITUTION -> -1)

  def score(c1: Char, c2: Char) = substMatrix match {
    case None => {
      (c1, c2) match {
        case ('-', '-') => values(MATCH)
        case ('-', d2) => values(INSERT)
        case (d1, '-') => values(INSERT)
        case (d1, d2) if (d1 == d2) => values(MATCH)
        case _ => values(SUBSTITUTION)
      }
    }
    case _ => 0 // TODO Read from matrix
  }
}