package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{MsgMatVecDone, MsgException}
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
 * @param getMatrix
 */
protected[concurrency] final class NoDepMatlabActor(getMatrix:() => Array[Array[Option[Double]]], range: Int)
  extends NoDepAbsActor[Double](getMatrix, range){


  override def eTerms = ETerms("Matrix conversion", "Row", "")
  //private val matrix: Array[Array[Double]] = Array.ofDim(getMatrix.length, getMatrix(0).length)


  override protected final def actReact{
    react{
      case MsgException(e, row, 0) => handleException(e, row, 0)

      case MsgMatVecDone(row, vector) =>
        congestionControl
        mlMatrix(row) = vector.toArray
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


  override protected def ackStart: Array[Array[Double]] = mlMatrix

  /*(sender: OutputChannel[Any]){
    sender ! MsgMatDone(matrix)
  } */

}
