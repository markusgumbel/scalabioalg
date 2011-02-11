package net.gumbix.dynpro

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object CellPosition extends Enumeration {
  type CellPosition = Value

  val MAXIMUM_VALUE = Value("Max. global value")
  val MAXIMUM_VALUE_LAST_ROW = Value("Max. last row")
  val MAXIMUM_VALUE_LAST_COLUMN = Value("Max. last column")
  val MAXIMUM_INDICES = Value("Max. indices, i.e. (n, m)")
}