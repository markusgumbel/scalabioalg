package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.{CostPair, MsgRow}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class MxLeftUpVecActor(mxActor: MxLeftUpActor, I: Int)
extends MxVecActor[ListBuffer[CostPair]](mxActor){

  //bc =: broadcast
  private val listenerList = new ListBuffer[MxLeftUpVecActor]()
  private var j = 0

  protected[actors] def registerListener(listener: MxLeftUpVecActor) {
    listenerList += listener
  }


  protected def broadcast(costPairs: ListBuffer[CostPair]){
    for(listener <- listenerList) listener ! costPairs
  }


  override protected def ePair = new EPair(I, j)


  override def act{
    //This block resets the current object to its initial state
    matrix = mxActor.getMatrix()
    def _andThen{
      j = 0
      channelList.clear
      listenerList.clear
    }

    val debug = 2
    val (costPairList, loopEnd) = (new ListBuffer[CostPair](), matrix(0).length)

    loop{ //loopWhile(j < loopEnd){
      val idx = Idx(I, j)//in this case I is constant
      //val (noneStateInvoked, channels, values) = getAccValues(idx, (idx: Idx) => idx.i)
      //this would work just as well. An example is provided in MxUpVecActor.scala
      val accValues = getAccValues(idx, (idx: Idx) => idx.i)
      if(accValues._1){//noneStateInvoked
        /*one or more "None states" have encounter ergo
        this actor will have to sleep for a while.*/

        registerTo(accValues._2)//channels

        react{//exclusively react on broadcasts
          case MsgRow(z, row) => matrix(z) = row //from master actor

          case costPairs: ListBuffer[CostPair] => //from another slave actor (peer)
            handlePeerBroadcast(costPairs, costPairs(0).idx.i)
            //debug block
            //if(I == debug+1) print(" <=== "+sender.toString.substring(37)+"(" + costPairs.length + ")")

        }
      }else{
        matrix(idx.i)(idx.j) = mxActor.calcNewAccValue(accValues._3)//values
        costPairList += CostPair(idx, matrix(idx.i)(idx.j))
        //if(I == a) print(idx + "=" + matrix(idx.i)(idx.j) + " ")

        val z = j + 1
        if(z % mxActor.bcSize == 0){
          broadcast(costPairList)
          //debug block
          //if(I == debug) print(" >>> "+this.toString.substring(37)+"(" + costPairList.length + ")")
        }


        if(z == loopEnd){
          if(z % mxActor.bcSize != 0) broadcast(costPairList)
          mxActor ! MsgRow(I, matrix(I))
          exit
        }
        j += 1
      }
    }andThen(_andThen)
  }

}
