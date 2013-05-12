package net.gumbix.dynpro.concurrency

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
 * The Concurrency object depends on it.
 */
protected[concurrency] trait IMatrixComputer{


  val intervalSize = 250


  /**
   * The method is supposed to be implemented by the following classes
   * -MatrixActor (done)
   * -NoDepMatlabMatrixActor (in progress)
   * -MatrixThread (not implemented yet)
   * -MatlabMatrixThread (not implemented yet)
   *
   * Its purpose in the "MatrixActor" and the "MatrixThread" classes
   * is to perform in the given order the following tasks:
   * 1- start all the slaves (cell threads or - actors),
   * 2- start the ONLY master (matrix thread or - actor),
   * 3- wait until the matrix is fully computed,
   * 4- return it.
   *
   * It's purpose in the "NoDepMatlabMatrixActor" and the "NoDepMatlabMatrixActor" classes
   * is simply to convert a 1D Array object of type Option[Double]
   * to a 1D Array object of type Double.
   *
   * @return
   *   if class is (MatrixActor | MatrixThread) a 2D Array object of type Option[Double]
   *   if class is (NoDepMatlabMatrixActor | MatlabMatrixThread) a 1D Array object of type Double
   *   */
  def computeMatrix: Any //Array[Array[Option[Double]]]

}
