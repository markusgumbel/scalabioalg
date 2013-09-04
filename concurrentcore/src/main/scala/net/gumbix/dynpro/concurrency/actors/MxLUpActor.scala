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
 * @param bcMailSize see MxActor.scala
 * @param getAccValues see MxActor.scala
 * @param calcCellCost see MxActor.scala
 */
protected[concurrency] final class MxLUpActor(
  slModAm: Int, val slModVecLen: Int, bcMailSize: Int,
  getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  calcCellCost:(Idx, Array[Double]) => Unit
)extends MxActor(bcMailSize, getAccValues, calcCellCost){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //val loopEnd = matrix(0).length

  //private val actors = ListBuffer[MxLUpVecActor]()


  override protected def actReact{
    react{
      case channels: ListBuffer[Int] =>
        /*
        during the registration the master actor is the man in the middle
        all further communications will be been proceeded between the slave actors
        hence the peer to peer communication model
        */
        channels.foreach(ch => try{
          val channel = actors(ch)
          channel.getState match{
            case scala.actors.Actor.State.Terminated => reply(WAKEUP)
            /*The actor is no longer computing, the cost it has computed should
            therefore be found in the matrix indirectly accessible to all slave actors.
            Its occurrence likelihood is a bit higher than that of the MxUpActor.*/

            case _ => channel.registerListener(sender.asInstanceOf[MxLUpVecActor])
          }
        }catch{case e: NoSuchElementException => reply(WAKEUP)})

      case i: Int =>
        print("[%s] ".format(i))
        actors - i //sender.asInstanceOf[MxLUpVecActor]
        congestionControl
        //this broadcast is received once a slave actor is done computing.

      case MsgException(e, constI, loopPointer) => handleException(e, constI, loopPointer)
    }
  }

  override protected def eTermKey = "Row"

  /**
   * @return
   */
  override protected val getPoolSize = PoolSize(slModAm, 0)

  /**
   * This method creates and starts one MatrixVectorActor.
   * @param I =: firstCoordinate The column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute @ first.
   */
  override protected def startNewSlMod(I: Int){
    //start a column computation with the current version of the matrix.
    val actor = new MxLUpVecActor(this, I)
    actors(I) = actor

    actor.start
  }



}
