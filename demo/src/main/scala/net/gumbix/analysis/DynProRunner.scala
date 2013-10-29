package net.gumbix.analysis

import actors.Actor
import collection.mutable.Map

import net.gumbix.bioinf.hmm.Viterbi
import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode, AlignmentStep}
import AlignmentStep._
import net.gumbix.dynpro.DynPro
import net.gumbix.dynpro.concurrency.{ConClass, ConMode, Stage, Messages}
import ConClass._
import ConMode._
import Messages._
import Stage._
import scala.util.Random

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 5:56 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[analysis] abstract class DynProRunner[SeqDT](elements: List[SeqDT]){
  /***** OVERRIDE ATTRIBUTES - START *****/
  protected val alphabet: String
  protected val states: String
  protected val transP: Array[Array[Double]]
  protected val emmP: Array[Array[Double]]
  /***** OVERRIDE ATTRIBUTES - END *****/


  /***** PRIVATE ATTRIBUTES - START *****/
  //private val map = Map(INSERT -> -2, DELETE -> -2, MATCH -> 1, SUBSTITUTION -> -1)

  /***** PRIVATE ATTRIBUTES - END *****/

  /***** PRIVATE CLASSES - START *****/
  /********** SOLUTION CLASSES - START **********/
  private class SolActor[Dt](actor: SolutionsActor[Dt], dp: DynPro[Dt]) extends Actor{def act{
    dp.solution
    actor ! DONE
  }}
  private class SolutionsActor[Dt](seq: DynPro[Dt], con: DynPro[Dt]) extends Actor{def act{react{
    case START =>
      val to = sender
      new SolActor(this, seq).start
      new SolActor(this, con).start

      var counter = 0
      loopWhile(counter < 2){react{case DONE => counter += 1}}andThen to ! DONE
  }}}
  /********** SOLUTION CLASSES - END **********/
  //Global Alignment
  private class SeqAlignment(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL)
  private class ConAlignment(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val config = setConfig(LEFT_UP, EVENT)
  }

  //Viterbi
  private class SeqViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP)
  private class ConViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT) //not to worry this isn't an error.
  }

  // If necessary include HERE the new dynamic programming extender classes (sequential and concurrent).
  /***** PRIVATE CLASSES _ END *****/


  /***** PRIVATE METHODS - START ******/
  private def getSeq(len: Long): String = (0.asInstanceOf[Long] until len).map(_ =>
    Random.shuffle(elements).head.toString
  ).mkString

  /**
   * This method is used to compute the respective solutions and harvest the time it required to do so.
   * @param seq The sequential DynPro extender
   * @param con The concurrent DynPro extender
   * @return
   */
  private def getDurations[Dt](seq: DynPro[Dt], con: DynPro[Dt])
  : (Map[Stage, Double], Map[Stage, Double]) = {
    //concurrently conRun the solutions
    val actor = new SolutionsActor(seq, con)
    actor.start
    actor !? START match{case DONE => }
    //once the solutions computed, get the durations
    (seq.getDurations, con.getDurations)
  }
  /***** PRIVATE METHODS - END ******/


  /***** METHODS - START *****/
  /**
   *
   * @param len
   * @return
   */
  def runGlobalAlignment(len: Long) = {
    val (s1, s2) = (getSeq(len), getSeq(len))
    getDurations[AlignmentStep](new SeqAlignment(s1, s2), new ConAlignment(s1, s2))
  }

  /**
   *
   * @param len
   * @return
   */
  def runViterbi(len: Long) = {
    val s = getSeq(len)
    getDurations[Int](new SeqViterbi(s), new ConViterbi(s))
  }

  // If necessary include HERE a new dynamic programming runner method.
  /***** METHODS - END *****/
}


protected[analysis] object DnaDynProRunner extends DynProRunner[Char](List('A', 'C', 'G', 'T')){
  override protected val (alphabet, states, transP, emmP) = (
    ViterbiFigures.alphabet, ViterbiFigures.states,
    ViterbiFigures.transP, ViterbiFigures.emmP
  )
}


protected[analysis] object RnaDynProRunner extends DynProRunner[Char](List('A', 'C', 'G', 'U')){
  //the override block is incomplete
  protected val alphabet: String = ???
  protected val states: String = ???
  protected val transP: Array[Array[Double]] = ???
  protected val emmP: Array[Array[Double]] = ???
}
