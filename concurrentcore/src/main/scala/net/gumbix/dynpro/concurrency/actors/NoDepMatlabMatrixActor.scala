package net.gumbix.dynpro.concurrency.actors


/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/1/13
 * Time: 8:28 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[concurrency] final class NoDepMatlabMatrixActor(mx: Array[Array[Option[Double]]], rowActorAmount: Int)
                              extends NoDepAbsActor[Double](mx, rowActorAmount){


  protected[concurrency] def this(mx: Array[Array[Option[Double]]]) = this(mx, mx.length)


  override def stage = "Matrix conversion"

  override protected[actors] def handleSubMatrix(subMXCell: Option[Double]) ={
    subMXCell match {
      case value: Some[Double] => value.get
      case _ => 0.0
    }
  }

  override protected[actors]  def getNewClassObject = this

}
