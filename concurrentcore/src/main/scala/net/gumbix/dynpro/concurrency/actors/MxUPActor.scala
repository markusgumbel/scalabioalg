package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{Debugger, MsgCol, MsgException, CostPair}
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
protected[concurrency] final class MxUpActor(
  getMatrix:() => Array[Array[Option[Double]]], bcSize: Int,
  getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double] ,
  calcNewAccValue:(Array[Double]) => Option[Double]
)extends MxActor(getMatrix, bcSize, getAccValues, calcNewAccValue){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //val loopEnd = matrix.length
  private val channelMap = Map[Int, MxUpVecActor]()
  //amount of slaves actors
  protected[actors] def slAm = getPoolSize.slMod


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
            channelMap(ch % slAm).registerListener(sender.asInstanceOf[MxUpVecActor], ch)
          }catch{
            case e: NoSuchElementException => //the actor has already been destroyed
              val costPairs = new ListBuffer[CostPair]
              for(i <- 0 to matrix.length) costPairs += CostPair(Idx(i, ch), matrix(i)(ch))
              reply(costPairs)
              //reply(MsgCostPairs(costPairs)) =: sender ! MsgCostPairs(costPairs)
              //this is an acknowledgement
          }
        }

      case MsgException(e, firstJ, loopPointer) =>
        handleException(e, firstJ, loopPointer)

      case MsgCol(mxj, costPairs) =>
        //this broadcast is received once a slave actor is done computing
        for(cp <- costPairs) matrix(cp.idx.i)(cp.idx.j) = cp.value
        channelMap -= mxj
        congestionControl
    }
  }


  override protected def eTermKey = "Column"


  /**
   * Slave module =: vector actor
   * @return
   */
  override protected def getPoolSize = {
    var slAm = matrix(0).length
    while(slAm > dMaxPoolSize) slAm /= 2

    PoolSize(slAm, 0)
  }


  /**
   * This method creates and starts one MatrixVectorActor.
   * @param firstJ =: firstCoordinate The column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute @ first.
   */
  override protected def startNewSlMod(firstJ: Int){
    //start a column computation with the current version of the matrix.
    val actor = new MxUpVecActor(this, firstJ)
    channelMap += (firstJ -> actor)
    slModules += actor
  }


  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   * @param j
   */
  override protected def restartSlMod(j: Int){//j ~= firstJ
    val actor = slModules(j).asInstanceOf[MxUpVecActor]
    channelMap += (j -> actor)
  }


  override protected def beforeLoopWhile{
    for(actor <- channelMap.values) try{ actor.restart
      }catch{ case e: IllegalStateException => actor.start
    }
  }

}
