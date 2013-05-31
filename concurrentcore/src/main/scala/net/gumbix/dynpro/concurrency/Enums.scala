package net.gumbix.dynpro.concurrency

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:27 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 * The ConClass is made of the 2 dependency "classes" used in the approaches
 * implemented and some additional "classes" increasing the flexibility of the
 * concurrency package.
 */
object ConClass extends Enumeration{
  type ConClass = Value

  private val (seq, con) = ("seq","con")
  private def stage(nr: String, txt: String)  = " \nstage "+ nr +" => "+ txt
  private def stageI(txt: String) = stage("I", txt)
  private def stageII(txt: String) = stage("II", txt)
  private def stageIII(txt: String) = stage("III", txt)
  private def stageIV(txt: String) = stage("IV", txt)

  //the first 2 dependency cases allow the algorithm to be computed concurrently
  val LEFT_UP = Value("left up:" + stageI(con) + stageII(con) + stageIII(con) + stageIV(con))
  val UP = Value("up:" + stageI(con) + stageII(con) + stageIII(con) + stageIV(con))

  val NO_CON = Value("no concurrency:" + stageI(seq) + stageII(seq) + stageIII(seq) + stageIV(seq))
  val LESS_CON = Value("less concurrency:" + stageI(con) + stageII(seq) + stageIII(con) + stageIV(seq))

  val NO_DEP = Value("no dependency:" + stageI(con) + stageII(seq) + stageIII(con) + stageIV(con))

}



object ConMode extends Enumeration{
  type ConMode = Value

  val EVENT = Value("event based concurrency")
  //event based concurrency

  val THREAD = Value("thread based concurrency")
  /*thread based concurrence
  * Side note: this type of concurrence is as well reachable with actors as with threads. */

  val SEQ = Value("no concurrency")
  //this value simply a placeholder when the DepCase is set to "NO_CON"

}


object Stage extends Enumeration{
  type Stage = Value

  val empty = Value("Stage i")
  val matrix = Value("Stage ii")
  val matlabMx = Value("Stage iii")
  val solution = Value("Stage iv")

}

