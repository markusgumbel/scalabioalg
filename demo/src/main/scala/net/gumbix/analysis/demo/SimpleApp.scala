package net.gumbix.analysis.demo

import net.gumbix.bioinf.hmm.Viterbi
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.bioinf.string.alignment.AlignmentStep._


/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/16/13
 * Time: 3:39 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object SimpleApp {
  private val (alphabet, states) = ("AGCT", "AGCTagct")//AGCTagct =: A+ G+ C+ T+ A- G- C- T-
  private val transP = Array(
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
  private val emmP = Array(
    Array(1.0, .0, .0, .0),//A+
    Array(.0, 1.0, .0, .0),//G+
    Array(.0, .0, 1.0, .0),//C+
    Array(.0, .0, .0, 1.0),//T+
    Array(1.0, .0, .0, .0),//A-
    Array(.0, 1.0, .0, .0),//G-
    Array(.0, .0, 1.0, .0),//C-
    Array(.0, .0, .0, 1.0) //T-
  )

  private val (s, s1, s2) =
    ("ACTCTCTCCTCCTCCTCACCTCATTGTCTCCCCGACTTATCCTAATGCGAAATTGGATTCTGAGCATTTT",
      "wiesengrund lampe bringen Meier Messer Ziele Straßburg",
      "schweinehund ampel trinken Maier Metzger Zeile Strassbourg")

  private object ConAlignment extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val config = setConfig(LEFT_UP, EVENT, true)
    override val values = Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)
  }

  private object ConViterbi extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT, true)
  }

  def main(args: Array[String]) {
    val toRun = 1
    val toPrint = toRun match{
      case 1 => ConAlignment.mkMatrixString(ConAlignment.solution)
      case 2 => ConViterbi.mkMatrixString(ConViterbi.solution)
    }

    println(toPrint)
    print("Done")
  }
}
