package net.gumbix.dynpro.concurrency.actors

import scala.Array
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.Debugger
import scala.actors.Actor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 2:34 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 *
 * @param getMatrix
 * @tparam MxDT
 */
protected[actors] abstract class NoDepAbsActor[MxDT](getMatrix:() => Array[Array[Option[Double]]], val range: Int)
  extends AbsMasterActor(getMatrix){

  override protected final def getPoolSize =
    PoolSize(matrix.length, matrix.length * (matrix(0).length/range + 1))


  override protected final def restartSlMod(row: Int){slModules(row).restart}


  override protected final def startNewSlMod(row: Int) {
    val actor = new NoDepRowActor[MxDT](this, row)
    actor.start
    slModules += actor
  }


  protected[actors] def handleCell(cell: Option[Double]): MxDT

  protected[actors] def sendMsg(row: Int, list: ListBuffer[MxDT])

}

