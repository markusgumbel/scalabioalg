package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.Idx


/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 3:06 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class increases the abstraction level for the master actor
 * used during the cost calculation stage.
 * @param _getDim see AbsMasterActor.scala
 * @param wuFreq see DynProConfig.scala
 * @param getAccValues see DynProConfig.scala
 * @param calcCellCost see DynProConfig.scala
 */
protected[actors] abstract class MxActor(
  _getDim:() => (Int, Int), val wuFreq: Int,
  val getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  val calcCellCost:(Idx, Array[Double]) => Unit
)extends AbsMasterActor(_getDim){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages

  /**
   * In this case a "round" correspond to one full invocation of
   * the MxActor.act method.
   * Problem: In case more than one computation of a certain dynamic programming
   * algorithm is planed, at least one of the slave actors will be start and later on restarted
   * for the round. This should be done in way that no message send in a previous round
   * interferes with one or more messages of the current version.
   * Unfortunately the access to the mailbox itself is restricted, hence the
   * round control used as a validation stamp. Each message sent between two slave actors (p2p) will be tagged
   * with the current round number. Once a round done all the messages still residing
   * in the mailboxes are automatically invalidated.
   */
  /*
  private var roundCounter = -1
  protected[actors] def getRound = roundCounter
  override protected def beforeReact{ roundCounter += 1 }
  */

  override protected def eTerms = ETerms("Cell evaluation", eTermKey, "Cell")

  //override protected def ackStart: MsgMxDone = MsgMxDone(matrix)
  override protected def ackStart: Symbol = net.gumbix.dynpro.concurrency.Messages.DONE

  protected def eTermKey: String
}
