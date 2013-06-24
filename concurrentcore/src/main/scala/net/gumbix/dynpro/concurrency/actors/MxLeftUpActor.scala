package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{MsgRow, MsgException}
import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.Idx
import scala.actors.Actor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[concurrency] final class MxLeftUpActor(
  getMatrix:() => Array[Array[Option[Double]]], bcSize: Int,
  getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double] ,
  calcNewAccValue:(Array[Double]) => Option[Double]
)extends MxActor(getMatrix, bcSize, getAccValues, calcNewAccValue){
  //val loopEnd = matrix(0).length
  private val channelMap = Map[Int, MxLeftUpVecActor]()

  override protected def actReact{
    react{
      case channels: ListBuffer[Int] =>
        /*
        during the registration the master actor is the man in the middle
        all further communications will be been proceeded between the slave actors
        hence the peer to peer communication model
        */
        for(ch <- channels){
          try{
            channelMap(ch).registerListener(sender.asInstanceOf[MxLeftUpVecActor])
          }catch{
            case e: NoSuchElementException => //the actor has already been destroyed
              reply(MsgRow(ch, matrix(ch)))
              //reply(MsgRow(ch, matrix(ch))) =: sender ! MsgRow(ch, matrix(ch))
              //this is an acknowledgement
          }
        }

      case MsgException(e, constI, loopPointer) =>
        handleException(e, constI, loopPointer)

      case MsgRow(mxi, row) =>
        //this broadcast is received once a slave actor is done computing
        matrix(mxi) = row
        channelMap -= mxi
        congestionControl
    }
  }


  override protected def eTermKey = "Row"

  /**
   * Slave module =: vector actor
   * @return
   */
  override protected def getPoolSize = PoolSize(getMatrix().length, 0)


  //override protected def beforeLoopWhile{print(matrix.length)}
  /**
   * This method creates and starts one MatrixVectorActor.
   * @param I =: firstCoordinate The column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute @ first.
   */
  override protected def startNewSlMod(I: Int){
    //start a column computation with the current version of the matrix.
    val actor = new MxLeftUpVecActor(this, I)
    channelMap += (I -> actor)
    actor.start
    slModules += actor
  }


  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   * @param I
   */
  override protected def restartSlMod(I: Int){//j ~= firstJ
    val actor = slModules(I).asInstanceOf[MxLeftUpVecActor]
    channelMap += (I -> actor)
    actor.restart
  }
}
