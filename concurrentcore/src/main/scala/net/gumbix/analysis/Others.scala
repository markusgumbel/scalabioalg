package net.gumbix.analysis

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/16/13
 * Time: 1:51 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object FactoringMode extends Enumeration{
  type FactoringMode = Value
  val ARI = Value("arithmetical growth")
  val GEO = Value("geometrical growth")
  val EXP = Value("exponential growth")
}


object DynPro extends Enumeration{
  type DynPro = Value
  val GLOBALG = Value("global alignment")
  val VITERBI = Value("viterbi")
}

object AnaVal extends Enumeration{
  type AnaVal = Value
  val MIN = Value("minimum")
  val MAX = Value("maximum")
  val AVG = Value("average")
  val MED = Value("median")
}
