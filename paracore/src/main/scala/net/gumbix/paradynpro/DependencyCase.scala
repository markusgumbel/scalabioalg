package net.gumbix.paradynpro

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/15/13
 * Time: 3:42 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object DependencyCase extends Enumeration{
  type DependencyCase = Value

  val LEFT_UPLEFT_UP = Value(1)
  val UPLEFT_UP_UPRIGHT = Value(2)
  val LEFT_UPLEFT_UP_UPRIGHT = Value(3)

  //the first 2 dependency cases allow the algorithm to be parallelized
}
