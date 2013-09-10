package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{Messages, MsgException}
import Messages.DONE
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

  //val loopEnd = matrix(0).length

  override protected val eTermKey = "Row"

  /**
   * @return
   */
  override protected val getPoolSize = PoolSize(slModAm, 0)

  /**
   * @param I The coordinate
   */
  override protected def getNewVecActor(I: Int) = new MxLUpVecActor(this, I)
  //NO actor.start

}
