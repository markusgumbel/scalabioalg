package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency.MsgMxDone

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 3:06 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] abstract class MxActor
(matrix: Array[Array[Option[Double]]],
 val getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double])
extends AbsMasterActor{
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages

  override protected def eTerms = ETerms("Cell evaluation", eTermKey, "Cell")

  /**
   * This method is used to start a slave actor.
   * @param c the (seemly) constant coordinate
   */
  override protected def startNewSlMod(c: Int){startNewSlMod(c, 0)}


  override protected def ackStart: MsgMxDone = MsgMxDone(matrix)


  protected def eTermKey: String
}
