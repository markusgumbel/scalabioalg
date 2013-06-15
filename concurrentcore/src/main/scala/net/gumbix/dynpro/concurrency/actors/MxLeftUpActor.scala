package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{MsgRow, MsgException}
import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/2/13
 * Time: 3:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[concurrency] final class MxLeftUpActor(
  matrix: Array[Array[Option[Double]]], bcSize: Int,
  getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double] ,
  calcNewAccValue:(Array[Double]) => Option[Double]
) extends MxActor(matrix, bcSize, getAccValues, calcNewAccValue){
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
        channelMap -= (mxi)
        congestionControl
    }
  }


  override protected def eTermKey = "Row"


  /**
   * Slave module =: vector actor
   * @return
   */
  override protected def getPoolSize = PoolSize(matrix.length, 0)


  /**
   * This method creates and starts one MatrixVectorActor.
   * @param constI =: constantCoordinate The row from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute.
   * @param loopStart The start position in the sub matrix.
   */
  override protected def startNewSlMod(constI: Int, loopStart: Int){
    //start a row computation with the current version of the matrix.
    //print(constI+ "-> ")
    val actor = new MxLeftUpVecActor(this, matrix, constI, loopStart)
    actor.start
    channelMap += (constI -> actor)
  }

}
