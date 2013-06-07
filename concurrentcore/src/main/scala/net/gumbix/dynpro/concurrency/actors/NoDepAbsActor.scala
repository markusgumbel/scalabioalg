package net.gumbix.dynpro.concurrency.actors

import scala.Array
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.Debugger

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
 * @param mx
 * @tparam MxDT
 */
protected[actors] abstract class NoDepAbsActor[MxDT] (mx: Array[Array[Option[Double]]], val range: Int)
  extends AbsMasterActor{

  //def this(n: Int, m: Int, noDepRowActorAm: Int) = this(Array.ofDim(n, m), noDepRowActorAm)


  override protected final def getPoolSize =
    PoolSize(mx.length, mx.length * (mx(0).length/range + 1))


  override protected final def startNewSlMod(row: Int) {
    new NoDepRowActor[MxDT](this, row, mx(row)).start
  }


  protected[actors] def handleCell(cell: Option[Double]): MxDT

  protected[actors] def sendMsg(row: Int, list: ListBuffer[MxDT])

}

