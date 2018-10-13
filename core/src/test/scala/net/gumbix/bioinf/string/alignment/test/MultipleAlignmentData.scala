package net.gumbix.bioinf.string.alignment.test

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait MultipleAlignmentData {
  val seqs = Map(
    "pair" -> Array("CAT", "AT"),
    "three1" -> Array("CAT", "AT", "THAT")
  )
}
