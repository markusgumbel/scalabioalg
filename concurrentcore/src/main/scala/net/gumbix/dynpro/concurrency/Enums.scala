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

  private val (seq, con, i, ii, m) = ("seq","con", "I", "II", "MatLab")
  private def stage(nr: String, txt: String)  = " \n\tstage "+ nr +" => "+ txt

  //the first 2 dependency cases allow the algorithm to be computed concurrently
  val LEFT_UP = Value("left up:" + stage(i, con) + stage(ii, con) + stage(m, con))
  val UP = Value("up:" + stage(i, con) + stage(ii, con) + stage(m, con))
  val NO_DEP = Value("no dependency:" + stage(i, seq) + stage(ii, con) + stage(m, con))
  //val NO_CON = Value("no concurrency:" + stage(i, seq) + stage(ii, seq) + stage(m, seq))

}


object ConMode extends Enumeration{
  type ConMode = Value

  val EVENT = Value("event based concurrency")
  //event based concurrency

  val THREAD = Value("thread based concurrency")
  /*thread based concurrence
  * Side note: this type of concurrence is as well reachable with actors as with threads. */

  //val __ = Value("no concurrency")
  //this value is simply a placeholder when the DepCase is set to "NO_CON"

}


object Stage extends Enumeration{
  type Stage = Value
  val TOTAL = Value("total")
  val MATRIX = Value("stageI")
  val MATLABMX = Value("matlabMatrix")
  val SOLUTION = Value("stageII")
}
