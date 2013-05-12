package net.gumbix.dynpro.concurrency

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:27 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[dynpro] object DependencyCase extends Enumeration{
  type DependencyCase = Value

  val LEFT_UPLEFT_UP_UPRIGHT = Value("left | up left | up |up right")
  val UPLEFT_UP_UPRIGHT = Value("up left | up | up right")
  val NO_DEPENDENCY = Value("only concurrent when no dependency involved")
  val NOT_CONCURRENT = Value("totally sequential")

  //the first 2 dependency cases allow the algorithm to be computed concurrently
}


protected[dynpro] object ConcurrencyMode extends Enumeration{
  type ConcurrencyMode = Value

  val EVENT = Value("event based concurrency") //event based concurrency
  var THREAD = Value("thread based concurrency")
  /*thread based concurrence
  * Side note: this type of concurrence is as well reachable with actors as with threads.
  * */
}

