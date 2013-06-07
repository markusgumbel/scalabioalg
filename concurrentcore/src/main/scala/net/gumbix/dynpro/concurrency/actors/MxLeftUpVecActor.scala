package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.{CostPair, MsgRow, MsgRegister, MsgCostPairs}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class MxLeftUpVecActor
(mxActor: MxLeftUpActor, matrix: Array[Array[Option[Double]]], I: Int, loopStart: Int)
extends MxVecActor[ListBuffer[CostPair]](mxActor, matrix){

  //bc =: broadcast
  private val listenerList = new ListBuffer[MxLeftUpVecActor]()
  private var j = loopStart


  protected[actors] def registerListener(listener: MxLeftUpVecActor) {
    listenerList += listener
  }


  protected def broadcast(costPairs: ListBuffer[CostPair]){
    for(listener <- listenerList) yield listener ! MsgCostPairs(costPairs)
  }


  override protected def ePair = new EPair(I, j)


  override def act{
    val (costPairList, loopEnd) = (new ListBuffer[CostPair](), matrix(0).length)

    loop{ //loopWhile(j < loopEnd){
      val idx = Idx(I, j)//in this case I is constant
      val (noneStateInvoked, channels, values) = getAccValues(idx, (idx: Idx) => idx.i)

      if(noneStateInvoked){
        /*one or more "None states" have encounter ergo
        this actor will have to sleep for a while.*/
        registerTo(channels)

        react{//exclusively react on broadcasts
          case MsgRow(z, row) => matrix(z) = row //from master actor

          case MsgCostPairs(costPairs) => //from another slave actor (peer)
            handlePeerBroadcast(costPairs, costPairs(0).idx.i)
            //if(I == a) print("[" + costPairs(0).idx.i + "] ")
        }
      }else{
        matrix(idx.i)(idx.j) = mxActor.calcNewAccValue(values)
        costPairList += CostPair(idx, matrix(idx.i)(idx.j))
        //if(I == a) print(idx + "=" + matrix(idx.i)(idx.j) + " ")

        val z = j + 1
        if(z % bcFreq == 0) broadcast(costPairList)

        if(z == loopEnd){
          if(z % bcFreq != 0) broadcast(costPairList)
          mxActor ! MsgRow(I, matrix(I))
          exit
        }

        j += 1
      }
    }
  }

}
