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


object ViterbiFigures{
  val (alphabet, states) = ("AGCT", "AGCTagct")//AGCTagct =: A+ G+ C+ T+ A- G- C- T-
  val transP = Array(
      Array[Double](0, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f),//fair cases
      //q0, A+,  G+,  C+,  T+,  A-,  G-,  C-,  T-
      Array(0, .20, .36, .25, .14, .01, .02, .01, .01),//A+
      Array(0, .17, .35, .29, .14, .01, .02, .02, .01),//G+
      Array(0, .19, .22, .34, .20, .01, .01, .02, .01),//C+
      Array(0, .09, .35, .30, .20, .00, .02, .02, .01),//T+
      Array(0, .01, .01, .01, .01, .28, .27, .20, .20),//A-
      Array(0, .01, .02, .01, .01, .24, .31, .22, .18),//G-
      Array(0, .02, .00, .02, .01, .31, .08, .29, .27),//C-
      Array(0, .01, .02, .01, .01, .18, .29, .23, .25) //T-
    )
  val emmP = Array(
    Array(1.0, .0, .0, .0),//A+
    Array(.0, 1.0, .0, .0),//G+
    Array(.0, .0, 1.0, .0),//C+
    Array(.0, .0, .0, 1.0),//T+
    Array(1.0, .0, .0, .0),//A-
    Array(.0, 1.0, .0, .0),//G-
    Array(.0, .0, 1.0, .0),//C-
    Array(.0, .0, .0, 1.0) //T-
  )
}


protected[analysis] case class GraphValues(
  lens: ListBuffer[Double],
  seqMin: ListBuffer[Double],
  seqMax: ListBuffer[Double],
  seqMed: ListBuffer[Double],
  seqAvg: ListBuffer[Double],
  conMin: ListBuffer[Double],
  conMax: ListBuffer[Double],
  conMed: ListBuffer[Double],
  conAvg: ListBuffer[Double]
){
  def getText = {
    val text = new StringBuilder
    for(i <- 0 until lens.length){
      if(i != 0) text.append("\n")
      text ++= "%s %s %s %s %s %s %s %s %s".format(
        lens(i), seqMin(i), seqMax(i), seqMed(i), seqAvg(i),
        conMin(i), conMax(i), conMed(i), conAvg(i)
      )
    }
    text.toString
  }
}