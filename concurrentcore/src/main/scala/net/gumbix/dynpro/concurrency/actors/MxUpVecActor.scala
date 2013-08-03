package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.Messages._
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/4/13
 * Time: 4:59 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class represents the slave actor using the column by column
 * approach during the cost calculation stage
 * @param mxActor see AbsSlaveActor.scala
 * @param J matrix(J) =: column vector to be computed by this slave actor
 */
protected[actors] final class MxUpVecActor(mxActor: MxUpActor, J: Int)
extends MxVecActor(mxActor){

  private var i = 0

  override protected def getIdxCoor(idx: Idx) = idx.j

  /**
   * This method should be used to set the values of the EPair case class.
   * @return
   */
  protected def ePair = new EPair(J, i)


  override def act{
    val loopEnd = mxActor.slModVecLen

    def afterLoopWhile{
      broadcast
      /**
      The broadcast frequency in this case can so far only be set to one
      if however in the future new algorithms are implemented
      accepting a frequency higher than one,
      use the following block instead of the block above.
      if(i % mxActor.wuFreq == 0 && loopEnd1 - i >= mxActor.wuFreq/2) broadcast
      */
      i += 1
    }

    loop{ //loopWhile(i < loopEnd){
      if(i == loopEnd){
        broadcast
        mxActor ! DONE
        exit
      }else{
        var _J = J //unlike J, _J is seemly constant
        loopWhile(_J < mxActor.slModAm){//innerLoopEnd = matrix(0).length
          val idx = Idx(i, _J)
          //val accValues = getAccValues(idx, (idx: Idx) => idx.i)
          //this would work just as well. An example is provided in MxLUpVecActorala
          val (nullStateInvoked, channels, values) = getAccValues(idx)

          if(nullStateInvoked){
            /*one or more "Null states" have encounter ergo
            this actor will have to sleep for a while.*/
            registerTo(channels)

            react{//exclusively react on broadcasts
              case WAKEUP => //simply wake up
            }
          }else{
            /*All required costs are available ergo the evaluation of the current
            can be proceeded.*/
            mxActor.calcCellCost(idx, values)
            _J += mxActor.slAm
          }
        }andThen afterLoopWhile
      }
    }
  }

}
