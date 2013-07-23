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

  override protected def reset = {
    j = 0
    super.reset
  }


  override def act{
    //This block resets the current object to its initial state
    reset
    val loopEnd = mxActor.getDim._2

    /*DEBUG PROTOCOL
    from mr. P. Meppe to Prof. M. Gumbel on the 10.juli.2013
    - the "debug" attribute represents the vector the developer is willing examine.
    - the "ifDebug" method is used to print out anything concerning the current "debug" value.
    - the "ifDebug" method is used to print out anything concerning the direct follower of the "debug" value.

    1- So far if the j index (see Idx(i, j)) appears in brackets e.g.: [3], it means the actor
    is about to be suspended.
    2- Right after that the following should appear "SENT('wakeSlaveModuleUp)RECEIVED('wakeSlaveModuleUp)". It means
    that the actor handling the vector prior to the current vector (debug value) (aka the chanel) sent a broadcast and
    the current actor received it and woke up.
    3- Once up the actor should print each j index appear in parenthesis e.g.: {3} until it encounters the next
    empty dependency value, causing him to print the j index in brackets and yield.
      In between (after 10 parenthesis) it sends a broadcasts to its listeners represented by "==>'wakeSlaveModuleUp".

    These steps should be repeated until the computation of the vector is done.

    To test the developer should run
    - net.gumbix.analysis.demo.MyDistanceApp.main for a manual repetition
    - net.gumbix.analysis.demo.DebugMultAlignApp.main for an automatic repetition
    */
    //debug block
    val debug = 5
    def debugToStr(str:Any) = str.toString.substring(37)
    def ifDebug(str: Any) = if(I == debug) print(str)
    def ifDebugg(str: Any) = if(I == debug + 1) print(str)

    loop{ //loopWhile(j < loopEnd){
      if(j == loopEnd){
        if(this.mailboxSize == 0){
          broadcast
          mxActor ! DONE
          exit
        }else react{
          case _ => //do nothing
          //all the messages have to be handled before the exit exception is thrown
        }
      }else{
        val idx = Idx(I, j)//in this case I is constant
        //val (noneStateInvoked, channels, values) = getAccValues(idx, (idx: Idx) => idx.i)
        //this would work just as well. An example is provided in MxUpVecActor.scala
        val accValues = getAccValues(idx)

        if(accValues._1){ifDebugg("["+j+"]")//nullStateInvoked
          /*one or more "Null states" have encounter ergo
          this actor will have to sleep for a while.*/
          registerTo(accValues._2)//channels

          react{//exclusively react on broadcasts
            case WAKEUP => ifDebugg("RECEIVED("+WAKEUP+")")//I am up now
          }
        }else{ifDebugg("{"+j+"}")
          mxActor.calcCellCost(idx, accValues._3)//values
          j += 1
          if(j % mxActor.wuFreq == 0 && loopEnd - j >= mxActor.wuFreq/2) broadcast(I, debug)
            /*The last sequence before the end will not automatically be broadcast*/
        }
      }
    }
  }

}
