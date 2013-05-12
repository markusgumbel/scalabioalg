package net.gumbix.dynpro.concurrency.actors

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 3:02 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[concurrency] final class NoDepEmptyMatrixActor(n: Int, m: Int, rowActorAmount: Int)
                  extends NoDepAbsActor[Option[Double]](Array.ofDim(n, m), rowActorAmount){

  /*subMatrix can not be replaced by null */

  protected[concurrency] def this(mx: Array[Array[Option[Double]]]) = this(mx, mx.length)


  override def stage = "Empty matrix creation"

  override protected[actors] def handleSubMatrix(subMXCell: Option[Double]) = None

  override protected[actors] def getNewClassObject = this
}
