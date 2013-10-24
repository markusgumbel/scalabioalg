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

  /**
   * @see <code>MxActor.broadcast</code>
   */
  protected final def broadcast = mxActor.broadcast(this)


  /**
   * @see <code>net.gumbix.dynpro.DynPro.getAccValues</code>
   * @return Boolean, Array[Double]
   */
  protected final def getAccValues(idx: Idx) = {
    //this attribute notifies the current extender to wait for the next broadcast.
    var noneStateInvoked = false

    /**
     * This (functional) method is used to link and handle the null state,
     * by registering the actor capable of solving the null state
     * and force the current actor to wait.
     * The Actor.SUSPENDED state as been set as the "waiting" state.
     * During one invocation of the mxActor.getAccValues(idx, handleNullState)
     * method it can be called 0, once or more once.
     * @param idx The index where the null state occured.
     */
    def handleNullState(idx: Idx){
      mxActor.link(getIdxCoor(idx), this)
      noneStateInvoked = true
      //once the "noneStateInvoked" is set to true (in the getAccValues method's scope)
      //it won't change its value again
    }

    val values = mxActor.getAccValues(idx, handleNullState)

    (noneStateInvoked, values)
  }


  /**
   * This method returns the (seemly) constant coordinate of the given index.
   * Idx.i for the "LEFT UP" dependency class.
   * Idx.j for the "UP" dependency class.
   * @param idx
   * @return
   */
  protected def getIdxCoor(idx: Idx): Int

}
