package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.{MsgCol, CostPair}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/4/13
 * Time: 4:59 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class MxUpVecActor
(mxActor: MxUpActor, matrix: Array[Array[Option[Double]]], firstJ: Int, loopStart: Int)
extends MxVecActor[Map[Int, ListBuffer[CostPair]]](mxActor, matrix){

  //bc =: broadcast
  private val listenerListMap = Map[Int, ListBuffer[MxUpVecActor]]()
  private var i = loopStart


  protected[actors] def registerListener(listener: MxUpVecActor, channel: Int) {
    try{
      listenerListMap(channel) += listener
    }catch{
      case e: NoSuchElementException => //the list hasn't been created yet
        listenerListMap(channel) = ListBuffer(listener)
    }
  }


  protected def broadcast(costPairsMap: Map[Int, ListBuffer[CostPair]]){
    for(cp <- costPairsMap){
      //cp = cost pair combination, channel = cp._1, broadcast = cp._2
      for(listener <- listenerListMap(cp._1))
        listener ! cp._2
    }
  }


  /**
   * This method should be used to set the values of the EPair case class.
   * @return
   */
  protected def ePair = new EPair(firstJ, i)


  override def act{
    val (costPairsMap, loopEnd1, loopEnd2) =
      (Map[Int, ListBuffer[CostPair]](), matrix.length, matrix(0).length)

    loop{ //loopWhile(i < loopEnd1){
      var j = firstJ
      loopWhile(j < loopEnd2){
        val idx = Idx(i, j) //in this case j is seemly constant
        val (noneStateInvoked, channels, values) = getAccValues(idx, (idx: Idx) => idx.j)

        if(noneStateInvoked){
          /*one or more "None states" have encounter ergo
          this actor will have to sleep for a while.*/
          registerTo(channels)

          react{//exclusively react on broadcasts
            case costPairs: ListBuffer[CostPair] =>
              if(sender.isInstanceOf[MxUpActor])//from master actor
                for(cp <- costPairs) matrix(cp.idx.i)(cp.idx.j) = cp.value

              else if(sender.isInstanceOf[MxLeftUpActor])//from another slave actor (peer)
                handlePeerBroadcast(costPairs, costPairs(0).idx.j)
          }
        }else{
          /*All required costs are available ergo the evaluation of the current
          can be proceeded.*/
          matrix(idx.i)(idx.j) = mxActor.calcNewAccValue(values)
          //if(firstJ == 14) print(" "+ idx + "=" + matrix(idx.i)(idx.j))

          try{//channel = idx.j
            costPairsMap(idx.j) += CostPair(idx, matrix(idx.i)(idx.j))
          }catch{
            case e: NoSuchElementException =>
              costPairsMap(idx.j) = ListBuffer(CostPair(idx, matrix(idx.i)(idx.j)))
          }
        }
        j += mxActor.slAm
      }

      broadcast(costPairsMap)

      if(i + 1 == loopEnd1){
        val costPairs = new ListBuffer[CostPair]()
        for(cps <- costPairsMap) costPairs ++= cps._2
        mxActor ! MsgCol(firstJ, costPairs)
        exit
      }

      /**
      The broadcast frequency in this case can so far only be set to one
      if however in the future new algorithms are implemented
      accepting a frequency higher than one,
      use the next block instead of the block above.

      val z = i + 1
      if(z % mxActor.bcSize == 0) broadcast(costPairsMap)

      if(z == loopEnd1){
        if(z % mxActor.bcSize != 0) broadcast(costPairsMap)

        val costPairs = new ListBuffer[CostPair]()
        for(cps <- costPairsMap) costPairs ++= cps._2
        mxActor ! MsgCol(firstJ, costPairs)
      }
      */

      i += 1
    }
  }

}
