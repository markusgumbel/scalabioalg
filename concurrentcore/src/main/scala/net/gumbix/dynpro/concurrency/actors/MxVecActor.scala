package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.Messages.WAKEUP
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 2:57 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class increases the abstraction level for the slave actors
 * used during the cost calculation stage.
 * @param mxActor see AbsSlaveActor.scala
 */
protected[actors] abstract class MxVecActor(mxActor: MxActor)
extends AbsSlaveActor(mxActor){


  protected val (channelList, listenerList) = (new ListBuffer[Int](), new ListBuffer[MxVecActor]())

  protected def registerTo(channels: ListBuffer[Int]) {
    if(channels.nonEmpty){
      mxActor ! channels //register listener to channels
      channelList ++= channels
    }
  }

  /**
   * This method is invoked the master actor (MxLUpActor) to
   * register a potential new listener to the channel of the current
   * slave actor.
   * @param listener
   */
  protected[actors] final def registerListener(listener: MxVecActor){
    listenerList += listener
  }


  protected final def broadcast{
    for(listener <- listenerList) listener.getState match{
      /*this matching is made to avoid unnecessary mails, by only waking up
        actors waiting in a react.*/
      case scala.actors.Actor.State.Suspended => listener ! WAKEUP
      case _ => //do nothing
    }
  }


  /**
   *
   * @param idx
   * @return
   */
  protected def getAccValues(idx: Idx) = {
    val channels = new ListBuffer[Int]()
    var noneStateInvoked = false

    //block =: inner method
    def handleNullState(idx: Idx){
      val channel = getIdxCoor(idx) //get the constant coordinate
      noneStateInvoked = true
      if(!(channelList.contains(channel) || channels.contains(channel)))
        channels += channel
    }

    val values = mxActor.getAccValues(idx, handleNullState)

    (noneStateInvoked, channels, values)
  }


  override protected def reset = {
    channelList.clear
    listenerList.clear
  }


  /**
   * This method returns the (seemly) constant coordinate of the given index.
   * Idx.i for the "LEFT UP" dependency class.
   * Idx.j for the "UP" dependency class.
   *
   * @param idx
   * @return
   */
  protected def getIdxCoor(idx: Idx): Int

}
