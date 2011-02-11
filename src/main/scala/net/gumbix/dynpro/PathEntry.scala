package net.gumbix.dynpro

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class PathEntry[Decision](
        val decision: Decision,
        val value: Double,
        val currCell: Idx,
        val prevCell: Array[Idx]) {
  override def toString = {
    val prev = prevCell match {
      case Array() => "none"
      case _ => prevCell.toString
    }
    "decision=" + decision + ", v=" + value.toString +
            ", curr=" + currCell.toString + ", prev=" + prev
  }
}
