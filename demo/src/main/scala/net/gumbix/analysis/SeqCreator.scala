package net.gumbix.analysis

import scala.collection.mutable.ListBuffer
import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode}
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.dynpro.concurrency.Stage._
import net.gumbix.bioinf.hmm.Viterbi
import net.gumbix.dynpro.concurrency.Debugger

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 5:56 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[analysis] class SeqCreator[SeqDT](elements: List[SeqDT]){
  //These values are to be overridden
  protected val (alphabet, states, transP: Array[Array[Double]], emmP: Array[Array[Double]], map) =
    ("", "", Array(Array(.0)), Array(Array(.0)), Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1))

  private val recordTime = true
  private object SeqAlignment extends Alignment(new StringBuilder(""), new StringBuilder(""), AlignmentMode.GLOBAL){
    override val (values, config) = (map, setConfig(NO_CON, __, recordTime))
  }
  private object ConAlignment extends Alignment(new StringBuilder(""), new StringBuilder(""), AlignmentMode.GLOBAL){
    override val (values, config) = (map, setConfig(LEFT_UP, EVENT, recordTime))
  }
  private object SeqViterbi extends Viterbi(new StringBuilder(""), alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(NO_CON, __, recordTime)
  }
  private object ConViterbi extends Viterbi(new StringBuilder(""), alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT, recordTime)
  }


  /**
   *
   * @param nrOfSeq
   * @param len
   * @return
   */
  def getSeqs(nrOfSeq: Int, len: Int): ListBuffer[String] = {
    val actor = new CreatorActor[SeqDT](elements, nrOfSeq, len)
    actor.start
    actor !? Message.start match{
      case sequences: ListBuffer[String] => sequences
    }
  }


  def runGlobalAlignment(s1: String, s2: String) = {
    SeqAlignment.updateXY(s1, s2)
    ConAlignment.updateXY(s1, s2)
    SeqAlignment.solution
    ConAlignment.solution
    //println(seq.getDurations + " / " + con.getDurations)
    (SeqAlignment.getDurations, ConAlignment.getDurations)
  }


  def runViterbi(s: String) = {
    SeqViterbi.updateS(s)
    ConViterbi.updateS(s)
    SeqViterbi.solution
    ConViterbi.solution
    //println(seq.getDurations + " / " + con.getDurations)
    (SeqViterbi.getDurations, ConViterbi.getDurations)
  }

}


protected[analysis] object DNASeqCreator
  extends SeqCreator[Char](List('A', 'C', 'G', 'T')){
  override protected val (alphabet, states) = ("AGCT", "AGCTagct")//AGCTagct =: A+ G+ C+ T+ A- G- C- T-
  override protected val transP = Array(
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
  override protected val emmP = Array(
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


protected[analysis] object RNASeqCreator
  extends SeqCreator[Char](List('A', 'C', 'G', 'U')){
  //the override block is missing
}
