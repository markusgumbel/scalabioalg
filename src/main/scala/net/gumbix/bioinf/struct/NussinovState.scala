package net.gumbix.bioinf.struct

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object NussinovState extends Enumeration {
  type NussinovState = Value

  val EXTEND_BEGINNING = Value("Beginning")
  val EXTEND_END = Value("End")
  val PAIR = Value("Pair")
  val SPLIT = Value("Split")
}