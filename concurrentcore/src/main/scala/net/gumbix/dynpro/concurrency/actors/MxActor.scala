package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.Idx
import scala.collection.mutable.{ListBuffer, Map}
import scala.actors.{Actor, TIMEOUT}
import Actor.State.{Terminated, Suspended}
import net.gumbix.dynpro.concurrency.Messages._
import net.gumbix.dynpro.Idx

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
  //trapExit = true; //receive all the exceptions from the cellActors in form of messages
  //actor states http://www.scala-lang.org/api/current/index.html#scala.actors.Actor$$State$

  /***** ATTRIBUTES - START ******/
  private val actors = new ListBuffer[MxVecActor]()

  /**
   *
   */
  private val syncActor = new Actor{
    def act{
      loop(
        reactWithin(100){ //0.1 sec
          case TIMEOUT =>
            actors.foreach(actor => actor.getState match {
              case Suspended => actor ! WAKEUP
              case _ => //do nothing
            })

          case DONE => exit
        }
      )
    }
  }
  /***** ATTRIBUTES - END ******/


  /***** CONSTRUCTOR - START *****/
  syncActor.start
  /***** CONSTRUCTOR - END *****/


  /***** OVERRIDDEN FINAL METHODS - START *****/
  protected[actors] final def getActor(ij: Int) = {
    val actor = actors(ij)
    actor.getState match{
      case scala.actors.Actor.State.Terminated => null
      /* To avoid a dead lock.
       * The dead lock will occur if right before being set as "SUSPENDED"
       * a VecActor links itself to another VecActor, that already is in the
       * "TERMINATED" state.
       */

      case _ => actor
    }
  }


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
    actors += getNewVecActor(ij)
  //NO actor.start


  /**
   * This loop was intentionally chosen.
   * The following loop condition "actor <- slModules" isn't adequate because
   * it is possible that the required amount of actors is less than the number of actors
   * allocated in the "slModules" list.
   */
  override protected final def beforeLoopWhile = actors.foreach(actor => actor.start)

  /***** OVERRIDDEN FINAL METHODS - END *****/


  /***** METHODS TO OVERRIDE - START *****/
  protected val eTermKey: String
  protected def getNewVecActor(ij: Int): MxVecActor
  /***** METHODS TO OVERRIDE - END *****/

}
