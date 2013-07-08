package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.MsgException
import net.gumbix.dynpro.concurrency.Messages._
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
/**
 * This class represents the master actor during the concurrent computation
 * with the row by row approach
 * @param _getDim see MxActor.scala
 * @param bcFreq see MxActor.scala
 * @param getAccValues see MxActor.scala
 * @param calcCellCost see MxActor.scala
 */
protected[concurrency] final class MxLUpActor(
  _getDim:() => (Int, Int), bcFreq: Int,
  getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  calcCellCost:(Idx, Array[Double]) => Unit
)extends MxActor(_getDim, bcFreq, getAccValues, calcCellCost){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //val loopEnd = matrix(0).length

  override protected def actReact{
    react{
      case channels: ListBuffer[Int] =>
        /*
        during the registration the master actor is the man in the middle
        all further communications will be been proceeded between the slave actors
        hence the peer to peer communication model
        */
        for(ch <- channels){
          val channel = slModules(ch).asInstanceOf[MxLUpVecActor]
          channel.getState match{
            case scala.actors.Actor.State.Terminated => reply(WAKEUP)
              //the actor is no longer computing
              //the likelihood of this case to happen is very very low

            case _ => channel.registerListener(sender.asInstanceOf[MxLUpVecActor])
          }
        }

      case DONE => congestionControl
        //this broadcast is received once a slave actor is done computing

      case MsgException(e, constI, loopPointer) => handleException(e, constI, loopPointer)
    }
  }

  override protected def eTermKey = "Row"

  /**
   * @return
   */
  override protected def getPoolSize = PoolSize(getDim._1, 0)

  /**
   * This method creates and starts one MatrixVectorActor.
   * @param I =: firstCoordinate The column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute @ first.
   */
  override protected def startNewSlMod(I: Int){
    //start a column computation with the current version of the matrix.
    val actor = new MxLUpVecActor(this, I)
    slModules += actor
    actor.start
  }

  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   * @param I
   */
  override protected def restartSlMod(I: Int){ slModules(I).restart }


}
