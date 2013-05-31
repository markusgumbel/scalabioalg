package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{msgEmpVecDone, msgException}
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
  extends NoDepAbsActor[Option[Double]](Array.ofDim(n, m), range){


  override def eTerms = ETerms("Empty matrix creation", "Row", "")

  private val matrix: Array[Array[Option[Double]]] = Array.ofDim(n, m)


  override protected final def actReact{
    react{
      case msgException(row, 0) => restartSlMod(row)

      case msgEmpVecDone(row, vector) =>
        congestionControl
        matrix(row) = vector.toArray
    }
  }


  override protected[actors] final def sendMsg(row: Int, list: ListBuffer[Option[Double]]){
    this ! msgEmpVecDone(row, list)
  }

  override protected[actors] final def handleCell(cell: Option[Double]) = None


  override protected[concurrency] final def getMatrix: Array[Array[Option[Double]]] = {
    preCompute
    matrix
  }

}
