package net.gumbix.paradynpro

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:27 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[paradynpro] object DependencyCase extends Enumeration{
  type DependencyCase = Value

  val LEFT_UPLEFT_UP_UPRIGHT = Value(1)
  val UPLEFT_UP_UPRIGHT = Value(2)
  val NOPARA = Value(3)

  //the first 2 dependency cases allow the algorithm to be parallelized
}


protected[paradynpro] object ParaType extends Enumeration{
  type ParaType = Value

  val ACTOR = Value(1) //event based concurrence
  var THREAD = Value(2)
  /*thread based concurrence
  * Side note: this type of concurrence is as well reachable with actors as with threads.
  * */
}

