package net.gumbix.paradynpro

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 10:46 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 * This trait should exclusively be implemented by the master. That means:
 * - the matrix thread or
 * - the matrix actor.
 *
 * The ParaWrapper object depends on it.
 */
trait IMatrixComputer{

  /**
   * This method's purpose is to perform in the given order the following tasks:
   * 1- start all the slaves (cell threads or - actors),
   * 2- start the ONLY master (matrix thread or - actor),
   * 3- wait until the matrix is fully computed,
   * 4- return it.
   *
   * @return a 2 dimensional Array of type Option[Double]
   */
  def computeMatrix: Array[Array[Option[Double]]]

}
