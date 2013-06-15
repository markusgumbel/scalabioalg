package net.gumbix.analysis

import scala.collection.mutable.ListBuffer
import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode}
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.dynpro.concurrency.Stage._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 5:56 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[analysis] class SeqCreator[SeqDT](elements: List[SeqDT]){

  def GetSequences(nrOfSeq: Int, len: Int): ListBuffer[String] = {
    val actor = new CreatorActor[SeqDT](elements, nrOfSeq, len)
    actor.start
    actor !? Message.start match {
      case sequences: ListBuffer[String] => sequences
    }
  }


  def runGlobalAlignment(s1: String, s2: String) = {
    val map = Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)

    object SeqAlignment extends Alignment(s1, s2, AlignmentMode.GLOBAL){
      override val config = setConfig(NO_CON, SEQ, true)
      override val values = map
    }

    object ConAlignment extends Alignment(s1, s2, AlignmentMode.GLOBAL){
      override val config = setConfig(LEFT_UP, EVENT, true)
      override val values = map
    }

    SeqAlignment.solution
    ConAlignment.solution

    (SeqAlignment.getRecordedTimes, ConAlignment.getRecordedTimes)
  }


  def runViterbi(s: String){

  }


}


protected[analysis] object DNASeqCreator
  extends SeqCreator[Char](List('A', 'C', 'G', 'T')){
  //so far simply extend
}


protected[analysis] object RNASeqCreator
  extends SeqCreator[Char](List('A', 'C', 'G', 'U')){
  //so far simply extend
}
