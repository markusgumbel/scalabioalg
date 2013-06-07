package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{MsgMatDone, MsgMatVecDone, MsgException}
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/1/13
 * Time: 8:28 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 *
 * @param mx
 */
protected[concurrency] final class NoDepMatlabActor(mx: Array[Array[Option[Double]]], range: Int)
  extends NoDepAbsActor[Double](mx, range){


  override def eTerms = ETerms("Matrix conversion", "Row", "")


  private val matrix: Array[Array[Double]] = Array.ofDim(mx.length, mx(0).length)


  override protected final def actReact{
    react{
      case MsgException(e, row, 0) => handleException(e, row, 0)

      case MsgMatVecDone(row, vector) =>
        congestionControl
        matrix(row) = vector.toArray
    }
  }


  override protected[actors] final def sendMsg(row: Int, list: ListBuffer[Double]){
    this ! MsgMatVecDone(row, list)
  }


  override protected[actors] def handleCell(cell: Option[Double]) ={
    cell match {
      case value: Some[Double] => value.get
      case _ => 0.0
    }
  }


  override protected def ackStart: MsgMatDone = MsgMatDone(matrix)

  /*(sender: OutputChannel[Any]){
    sender ! MsgMatDone(matrix)
  } */

}
