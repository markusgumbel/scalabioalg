package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.MsgException
import net.gumbix.dynpro.concurrency.Messages._
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
  _getDim:() => (Int, Int), wuFreq: Int,
  getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  calcCellCost:(Idx, Array[Double]) => Unit
)extends MxActor(_getDim, wuFreq, getAccValues, calcCellCost){
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //val loopEnd = matrix.length
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
          val channel = slModules(ch % slAm).asInstanceOf[MxUpVecActor]
          channel.getState match{
            case scala.actors.Actor.State.Terminated => reply(WAKEUP)
            /*The actor is no longer computing, the cost it has computed should
            therefore be found in the matrix indirectly accessible to all slave actors.
            However the likelihood of this case to happen is very very low.*/

            case _ => channel.registerListener(sender.asInstanceOf[MxUpVecActor])
          }
        }

      case DONE => congestionControl
        //this broadcast is received once a slave actor is done computing

      case MsgException(e, firstJ, loopPointer) => handleException(e, firstJ, loopPointer)
    }
  }


  override protected def eTermKey = "Column"


  /**
   * Slave module =: vector actor
   * @return
   */
  override protected def getPoolSize = {
    var slAm = getDim._2
    while(slAm > dMaxPoolSize) slAm /= 2

    PoolSize(slAm, 0)
  }


  /**
   * This method creates and starts one MatrixVectorActor.
   * @param firstJ =: firstCoordinate The column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute @ first.
   */
  override protected def startNewSlMod(firstJ: Int){
    slModules += new MxUpVecActor(this, firstJ)
    //no start
  }


  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   * @param j
   */
  override protected def restartSlMod(j: Int){//j ~= firstJ
    //no restart
  }

  /**
   * In this dependency case the actor objects can't be started directly.
   * They first have to be created (hence "startSlMod" and "restartSlMod").
   * Once the creation stage done they can be (re)started.
   */
  override protected def beforeLoopWhile{
    /*the loop condition was intentionally chosen. The following loop condition "actor <- slModules" isn't
     adequate because it is possible that the required amount of actors is less than the number of actors
     allocated in the "slModules" list.
    */
    for(j <- 0 until slAm) try{ slModules(j).restart
    }catch{ case e: IllegalStateException => slModules(j).start
    }
  }

}
