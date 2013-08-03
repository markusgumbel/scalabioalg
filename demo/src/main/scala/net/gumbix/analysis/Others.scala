package net.gumbix.analysis

import scala.collection.mutable.ListBuffer

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
  val GLOBALG = Value("globalAlignment")
  val VITERBI = Value("viterbi")
}


protected[analysis] case class GraphValues(
  lens: ListBuffer[Long],
  seqMin: ListBuffer[Long],
  seqMax: ListBuffer[Long],
  seqMed: ListBuffer[Long],
  seqAvg: ListBuffer[Long],
  conMin: ListBuffer[Long],
  conMax: ListBuffer[Long],
  conMed: ListBuffer[Long],
  conAvg: ListBuffer[Long]
){
  def getText = {
    val (s, text) = (" ", new StringBuilder())
    for(i <- 0 until lens.length){
      if(i != 0) text.append("\n")
      text.append(lens(i)+s+seqMin(i)+s+seqMax(i)+s+seqMed(i)+s+seqAvg(i)+s+conMin(i)+s+conMax(i)+s+conMed(i)+s+conAvg(i))
    }
    text.toString
  }
}