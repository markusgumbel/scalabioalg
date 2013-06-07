package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.{CostPair, MsgRegister}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 2:57 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] abstract class MxVecActor[Broadcast]
(mxActor: MxActor, matrix: Array[Array[Option[Double]]])
extends AbsSlaveActor(mxActor){


  protected val (bcPointerMap, channelList, bcFreq) =
    (Map[Int, Int](), new ListBuffer[Int](), 10)


  protected def registerTo(channels: ListBuffer[Int]) {
    if(channels.nonEmpty){
      mxActor ! MsgRegister(channels) //register listener to channels
      channelList ++= channels
    }
  }


  /**
   *
   * @param idx
   * @param getIdxCoor get the (seemly) constant coordinate.
   *                   Idx.i for the "LEFT UP" dependency class.
   *                   Idx.j for the "UP" dependency class.
   * @return
   */
  protected def getAccValues(idx: Idx, getIdxCoor:(Idx) => Int) = {
    val channels = new ListBuffer[Int]()
    var noneStateInvoked = false

    //block =: inner method
    def handleNoneState(idx: Idx){
      val channel = getIdxCoor(idx) //get the constant coordinate
      noneStateInvoked = true
      if(!(channelList.contains(channel) || channels.contains(channel))){
        channels += channel
        bcPointerMap += (channel -> 0)
      }
    }

    val values = mxActor.getAccValues(matrix, idx, handleNoneState)

    (noneStateInvoked, channels, values)
  }


  /**
   * This methods enables the storage of messages coming from one or more
   * channels.
   * @param costPairs
   * @param Z
   */
  protected def handlePeerBroadcast(costPairs: ListBuffer[CostPair], Z: Int){
    var z = bcPointerMap(Z)
    while(z < costPairs.length){
      val cp = costPairs(z)
      matrix(cp.idx.i)(cp.idx.j) = cp.value
      z += 1
    }
    bcPointerMap += (Z -> z)
  }


  protected def broadcast(toBC: Broadcast)

}
