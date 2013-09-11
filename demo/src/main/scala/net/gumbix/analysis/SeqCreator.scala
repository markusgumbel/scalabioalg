package net.gumbix.analysis

import scala.collection.mutable.ListBuffer
import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode}
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
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
protected[analysis] abstract class SeqCreator[SeqDT](elements: List[SeqDT]){
  //These values are to be overridden
  protected val map = Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)

  protected val alphabet: String
  protected val states: String
  protected val transP: Array[Array[Double]]
  protected val emmP: Array[Array[Double]]

  private class SeqAlignment(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val values = map
  }
  private class ConAlignment(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val (values, config) = (map, setConfig(LEFT_UP, EVENT))
  }
  private class SeqViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP)

  private class ConViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT) //not to worry this isn't an error.
  }


  /**
   *
   * @param nrOfSeq
   * @param len
   * @return
   */
  def getSeqs(nrOfSeq: Int, len: Long): ListBuffer[String] = {
    val actor = new CreatorActor[SeqDT](elements, nrOfSeq, len)
    actor.start
    actor !? Message.start match{
      case sequences: ListBuffer[String] => sequences
    }
  }


  def runGlobalAlignment(s1: String, s2: String) = {
    val seq = new SeqAlignment(s1, s2)
    val con = new ConAlignment(s1, s2)
    seq.solution
    con.solution
    //println(seq.getDurations + " / " + con.getDurations)
    (seq.getDurations, con.getDurations)
  }


  def runViterbi(s: String) = {
    val seq = new SeqViterbi(s)
    val con = new ConViterbi(s)

    seq.solution
    con.solution //not to worry this isn't an error.
    //println(seq.getDurations + " / " + con.getDurations)
    (seq.getDurations, con.getDurations) //not to worry this isn't an error.
  }

}


protected[analysis] object DNASeqCreator extends SeqCreator[Char](List('A', 'C', 'G', 'T')){
  override protected val (alphabet, states, transP, emmP) = (
    ViterbiFigures.alphabet, ViterbiFigures.states,
    ViterbiFigures.transP, ViterbiFigures.emmP
  )
}


protected[analysis] object RNASeqCreator extends SeqCreator[Char](List('A', 'C', 'G', 'U')){
  //the override block is missing
  protected val alphabet: String = ???
  protected val states: String = ???
  protected val transP: Array[Array[Double]] = ???
  protected val emmP: Array[Array[Double]] = ???
}
