package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.Messages._
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency.Debugger

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class represents the slave actor using the row by row approach
 * during the cost calculation stage
 * @param mxActor see MxVecActor.scala
 * @param I matrix(I) =: row vector to be evaluated by this slave actor
 */
protected[actors] final class MxLUpVecActor(mxActor: MxLUpActor, I: Int)
extends MxVecActor(mxActor){

  private var j = 0

  override protected def getIdxCoor(idx: Idx) = idx.i

  override protected def ePair = new EPair(I, j)


  override def act{
    val loopEnd = mxActor.slModVecLen
    loop{ //loopWhile(j < loopEnd){
      if(j == loopEnd){
        broadcast
        mxActor ! DONE //this message is sent once this actor is done evaluating its vector
        exit
      }else{
        val idx = Idx(I, j)//in this case I is constant
        //val (noneStateInvoked, values) = getAccValues(idx, (idx: Idx) => idx.i)
        //this would work just as well. An example is provided in MxUpVecActor.scala
        val accValues = getAccValues(idx)

        if(accValues._1){//nullStateInvoked
          /* One or more "Null states" have encounter ergo
           * this actor will have to be SUSPENDED for a while.
           */
          react{//exclusively react on broadcasts
            case WAKEUP => //I am no longer in a "SUSPENDED" state.
          }
        }else{
          /* All required costs are available ergo the evaluation of the current
           * can be proceeded.
           */
          mxActor.calcCellCost(idx, accValues._2)//values
          j += 1
          if(j % mxActor.bcMailSize == 0 && loopEnd - j >= mxActor.bcMailSize/2) broadcast
          /*The last sequence before the end will not automatically be broadcast*/
        }
      }
    }
  }

}
