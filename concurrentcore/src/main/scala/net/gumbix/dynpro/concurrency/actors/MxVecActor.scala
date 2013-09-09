package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.ListBuffer
import scala.actors.Actor.State._
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency.Messages.WAKEUP

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

  //the listenerList is used during the broadcast.
  private val listeners = new ListBuffer[MxVecActor]()


  /**
   * This method is invoked other neighboring slave actors (MxLUpActor) to
   * let their self register as listener of the current slave actor.
   * If the registration is successful both slave actors become by definition
   * peer actors.
   * @param newListener
   * @return True if the listener is up to the invocation of the method unknown to
   *         the current slave actor, false otherwise.
   */
  protected[actors] final def registerListener(newListener: MxVecActor) =
    if(!listeners.contains(newListener)){
      listeners += newListener
      true
    }else false




  protected final def broadcast{
    /* This matching is made to avoid unnecessary mails, by only waking up
     * actors waiting in a react.
     */
    listeners.foreach(listener => listener.getState match{
      case New|Runnable|Terminated => //do nothing
      case _ => listener ! WAKEUP
    })
  }


  /**
   *
   * @param idx
   * @return
   */
  protected def getAccValues(idx: Idx) = {
    var noneStateInvoked = false

    //block =: inner method
    /**
     * This method is used to register and handle the null state,
     * by registering the actor capable of solving the null state
     * and force the current actor to wait.
     * During one invocation of the mxActor.getAccValues(idx, handleNullState)
     * method it can be called 0, once or more once.
     * @param idx The index where the null state occured.
     */
    def handleNullState(idx: Idx){
      val channel = mxActor.getActor(getIdxCoor(idx))
      if(channel != null){ //the actor is still computing
        noneStateInvoked = true
        //notify the current extender to wait for the next broadcast.
        //The Actor.SUSPENDED state as been set as the "waiting" state.

        channel.registerListener(this)
      }
    }

    val values = mxActor.getAccValues(idx, handleNullState)

    (noneStateInvoked, values)
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
