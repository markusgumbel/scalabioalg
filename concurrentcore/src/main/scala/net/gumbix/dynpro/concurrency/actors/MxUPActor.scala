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
protected[concurrency] final class MxUpActor(
  val slModAm: Int, val slModVecLen: Int, bcMailSize: Int,
  getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  calcCellCost:(Idx, Array[Double]) => Unit
)extends MxActor(bcMailSize, getAccValues, calcCellCost){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //val loopEnd = matrix.length
  //private val actors = ListBuffer[MxUpVecActor]()


  //amount of slaves actors
  protected[actors] lazy val slAm = getPoolSize.slMod


  override protected def actReact{
    react{
      case DONE => congestionControl
      //this broadcast is received once a slave actor is done computing

      case MsgException(e, firstJ, loopPointer) => handleException(e, firstJ, loopPointer)
    }
  }


  override protected val eTermKey = "Column"


  /**
   * Slave module =: vector actor
   * @return
   */
  override protected val getPoolSize = {
    var slAm = slModAm
    while(slAm > dMaxPoolSize) slAm /= 2

    PoolSize(slAm, 0)
  }


  /**
   * @param firstJ The first coordinate.
   */
  override protected def getNewVecActor(firstJ: Int) = new MxUpVecActor(this, firstJ)
    //NO actor.start

}
