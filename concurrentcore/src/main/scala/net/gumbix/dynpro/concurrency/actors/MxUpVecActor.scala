package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.{Messages, Debugger, MsgCol, CostPair}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/4/13
 * Time: 4:59 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class MxUpVecActor(mxActor: MxUpActor, firstJ: Int)
extends MxVecActor[Map[Int, ListBuffer[CostPair]]](mxActor){

  //bc =: broadcast
  private val listenerListMap = Map[Int, ListBuffer[MxUpVecActor]]()
  private var i = 0


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
      try{for(listener <- listenerListMap(cp._1)) listener ! cp._2}
      catch{case e: NoSuchElementException => }//there is no listener yet
    }
  }


  /**
   * This method should be used to set the values of the EPair case class.
   * @return
   */
  protected def ePair = new EPair(firstJ, i)


  override def act{
    //This block resets the current object to it's initial state
    matrix = mxActor.getMatrix()
    def _andThen{
      i = 0
      channelList.clear
      listenerListMap.clear
    }


    val (costPairsMap, loopEnd1, loopEnd2) =
      (Map[Int, ListBuffer[CostPair]](), matrix.length, matrix(0).length)

    def afterLoopWhile{
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

    loop{ //loopWhile(i < loopEnd1){
      var j = firstJ
      loopWhile(j < loopEnd2){
        val idx = Idx(i, j) //in this case j is seemly constant
        //val accValues = getAccValues(idx, (idx: Idx) => idx.i)
        //this would work just as well. An example is provided in MxLeftUpVecActor.scala
        val (noneStateInvoked, channels, values) = getAccValues(idx, (idx: Idx) => idx.j)

        if(noneStateInvoked){
          /*one or more "None states" have encounter ergo
          this actor will have to sleep for a while.*/
          registerTo(channels)
          //print(j + "(" +i+ ")-SLEEP  ")
          react{//exclusively react on broadcasts
            case costPairs: ListBuffer[CostPair] =>
              if(sender.isInstanceOf[MxUpActor])//from master actor
                for(cp <- costPairs) matrix(cp.idx.i)(cp.idx.j) = cp.value

              else if(sender.isInstanceOf[MxUpVecActor]){//from another slave actor (peer)
                handlePeerBroadcast(costPairs, costPairs(0).idx.j)
                //print("\n" + j + "(" +i+ ")-AWAKE  ")
              }

            case Messages.wakeUp => //simply wake up
          }
        }else{
          /*All required costs are available ergo the evaluation of the current
          can be proceeded.*/
          matrix(idx.i)(idx.j) = mxActor.calcNewAccValue(values)

          //channel = idx.j
          try{costPairsMap(idx.j) += CostPair(idx, matrix(idx.i)(idx.j))}
          catch{ case e: NoSuchElementException =>
            costPairsMap(idx.j) = ListBuffer(CostPair(idx, matrix(idx.i)(idx.j)))
          }
        }
        j += mxActor.slAm
      }andThen(afterLoopWhile)
    }andThen(_andThen)
  }

}
