package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.Idx
import scala.collection.mutable.ListBuffer
import scala.actors.Actor


/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 3:06 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] abstract class MxActor
(getMatrix:() => Array[Array[Option[Double]]], val bcSize: Int,
 val getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double],
 val calcNewAccValue:(Array[Double]) => Option[Double])
extends AbsMasterActor(getMatrix){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages

  override protected def eTerms = ETerms("Cell evaluation", eTermKey, "Cell")


  //override protected def ackStart: MsgMxDone = MsgMxDone(matrix)
  override protected def ackStart: Array[Array[Option[Double]]] = matrix

  protected def eTermKey: String
}
