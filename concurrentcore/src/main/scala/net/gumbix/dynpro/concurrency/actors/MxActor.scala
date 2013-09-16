package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import scala.actors.{Actor, TIMEOUT}
import Actor.State._

import net.gumbix.dynpro.concurrency.Messages._
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency.MsgException

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/6/13
 * Time: 3:06 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class increases the abstraction level for the master actor
 * used during the cost calculation stage.
 * @param bcMailSize see DynProConfig.scala
 * @param getAccValues see DynProConfig.scala
 * @param calcCellCost see DynProConfig.scala
 */
protected[actors] abstract class MxActor(
  val bcMailSize: Int,
  val getAccValues:(Idx, Idx => Unit) => Array[Double] ,
  val calcCellCost:(Idx, Array[Double]) => Unit
)extends AbsMasterActor{
  //actor states http://www.scala-lang.org/api/current/index.html#scala.actors.Actor$$State$

  /**
   * trapExit = true;
   * This is used to receive all the exceptions from the MxVecActors in form of messages.
   * But for some reason it cause an exception itself... I know right :)
   * That's why i implemented the exception handler myself.
   * @see <code>net.gumbix.dynpro.concurrency.actors.AbsSlaveActor.exceptionHandler</code>
   */


  /***** ATTRIBUTES - START ******/
  private val channels = Map[MxVecActor, ListBuffer[MxVecActor]]() //actors -> its listeners

  /**
   * This attribute (actor) takes care of synchronising the MxVecActor's.
   * It has an internal TIMEOUT. Once it expires right before resetting itself it "WAKES UP"
   * all the currently suspended MxVecActor's.
   * @note It has to use the "lazy" attribute avoid initializing itself when the "channels"
   *       is still empty.
   */
  private lazy val syncActor = new Actor{
    val (len, millis) = (channels.size, (1e4 / (2.6 * 1e6) + 1).asInstanceOf[Long])
    /* 1GHz ~= 1e9 cycles/second (computer instructions/second)
     * Unlike c/c++
     *
     * The following formula is used.
     * millis [ms] = desired # of cycles before the syncActor's TIMEOUT expires / (CPU speed [GHz] * 1e6)
     *
     * The original formula is:
     * millis [ms] = (desired # of cycles before the syncActor's TIMEOUT expires * 1e3) / (CPU speed [GHz] * 1e9)
     *
     * Currently the syncActor wakes up all sleeping actors periodically after approximately 1e4 cycles.
     */
    def act{
      loop(
        reactWithin(scala.math.max(millis, 1)){
          case TIMEOUT =>
            val _channels = channels.toList
            new BoastAsyncActor(len, {i =>
              val actor = _channels(i)._1
              actor.getState match{
                case Suspended => actor ! WAKEUP
                case _ => //do nothing
              }
            }).start

          case DONE => exit
        }
      )
    }
  }
  /***** ATTRIBUTES - END ******/


  /***** FINAL METHODS - START *****/
  /**
   *
   * @param ij The channel coordinate
   * @param listener The new listener
   * @return
   */
  protected[actors] final def link(ij: Int, listener: MxVecActor): Boolean = {
    val channel = channels.toList(ij)
    channel._1.getState match{
      case Terminated => false
      /* To avoid a dead lock.
       * The dead lock will occur if right before being set as "SUSPENDED"
       * a VecActor links itself to another VecActor, that already is in the
       * "TERMINATED" state.
       */

      case _ => //all other states will theoretically @ least send one broadcast after the linking (below)
        val listeners = channel._2
        if(!listeners.contains(listener)) listeners += listener
        //there's no need to have the same actor more than once in the same list.
        true
    }
  }

  /**
   *
   * @param actor
   */
  protected[actors] final def broadcast(actor: MxVecActor) = channels(actor).foreach(
    /* This matching is made to avoid unnecessary mails, by only waking up
     * actors waiting in a react.
     */
    listener => listener.getState match{
      case New|Runnable|Terminated => //do nothing
      case _ => listener ! WAKEUP
    }

    /* In this case for now the number of listener pro MxVecActor is still in scope.
     * Making the usage of the SpeedUpActor unnecessary.
     */
  )
  /***** FINAL METHODS - END *****/


  /***** OVERRIDDEN FINAL METHODS - START *****/
  override protected final def eTerms = ETerms("Cell evaluation", eTermKey, "Cell")

  override protected final def ackStart: Symbol = {
    syncActor ! DONE
    DONE
  }

  /**
   * This method creates one MxVectorActor with the current version of the matrix.
   * @param ij The row- / first column- from the original matrix considered as the sub matrix that
   *           the new MxVectorActor will compute.
   * @note To enable an (indirect) access right from the start
   *       from each VecActor to the collectivity,
   *       unlike its equivalents in the MatlabActor.scala and the SolutionActor.scala,
   *       this method solely creates the slave (peer) actors.
   *       Their startup is done in the method below called "beforeLoopWhile".
   */
  override protected final def iniNewSlMod(ij: Int) =
    channels(getNewVecActor(ij)) = new ListBuffer[MxVecActor]() //NO actor.start

  /**
   * This loop was intentionally chosen.
   * The following loop condition "actor <- slModules" isn't adequate because
   * it is possible that the required amount of actors is less than the number of actors
   * allocated in the "slModules" list.
   */
  override protected final def beforeLoopWhile{
    val _channels = channels.toList
    new BoastAsyncActor(channels.size, i => _channels(i)._1.start).start //start all the MxVecActor's
    syncActor.start
  }

  override protected final def actReact{
    react{
      case DONE => congestionControl //this broadcast is received once a slave actor is done computing
      case MsgException(e, ij, loopPointer) => handleException(e, ij, loopPointer)
    }
  }
  /***** OVERRIDDEN FINAL METHODS - END *****/


  /***** METHODS TO OVERRIDE - START *****/
  protected val eTermKey: String
  protected def getNewVecActor(ij: Int): MxVecActor
  /***** METHODS TO OVERRIDE - END *****/

}
