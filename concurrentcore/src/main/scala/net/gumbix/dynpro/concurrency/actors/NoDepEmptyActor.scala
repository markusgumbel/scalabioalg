package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{MsgEmpVecDone, MsgException}
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 3:02 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 *
 * @param n
 * @param m
 */
protected[concurrency] final class NoDepEmptyActor(n: Int, m: Int, range: Int)
  extends NoDepAbsActor[Option[Double]](() => Array.ofDim(n, m), range){


  override def eTerms = ETerms("Empty matrix creation", "Row", "")
  //private val matrix: Array[Array[Option[Double]]] = Array.ofDim(n, m)


  override protected def actReact{
    react{
      case MsgException(e, row, 0) => handleException(e, row, 0)

      case MsgEmpVecDone(row, vector) =>
        congestionControl
        matrix(row) = vector.toArray
    }
  }


  override protected[actors] def sendMsg(row: Int, list: ListBuffer[Option[Double]]){
    this ! MsgEmpVecDone(row, list)
  }

  override protected[actors] def handleCell(cell: Option[Double]) = None


  override protected def ackStart: Array[Array[Option[Double]]] = matrix

}
