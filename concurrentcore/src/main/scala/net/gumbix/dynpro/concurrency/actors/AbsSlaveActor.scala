package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency.MsgException

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 7:22 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class is a part of each slave actor in this prototype.
 * It simplifies the implementation of each specific slave actor
 * by creating an abstraction level.
 * @param maAc the master actor
 */
protected[actors] abstract class AbsSlaveActor(maAc: AbsMasterActor)
  extends Actor{

  /*
  This way all MasterActor objects will crash if their
  SlaveActor crashes. That way we save resources.
  */
  //link(master)


  /* Counter
  The counter is used to constantly have an overview on the current amount of
  internal actors.
  Once their work done there is no further need to keep the "loop" (iterating them)
  alive.
  Caution:
  The counter has to be initialized with -1 cuz besides being the overview instance
  it's used as the key in in the MatlabVecActor.startInternalActors.pairMap .
  */
  private var counter = -1
  protected def getCounter = counter
  protected def raiseCounter = counter += 1
  protected def reduceCounter = counter -= 1
  protected def keepLoopAlive = counter > -1

  /**
   * This method should be used to start all the internal actors.
   * in this context the term "internal actors"
   * means private slave actors to the current slave actor.
   *
   * CAUTION:
   * This method isn't abstract on purpose. Not all slave actors might need
   * internal actors.
   */
  protected def startInternalActors{}


  /**********Exception handlers - START**********/
  /**
   * This case class stores all values necessary to be restart a crashed slave actor.
   * @param key The unique identifier given to the slave actor by it's master object.
   * @param pointer The position right before the crash (TO USE if necessary)
   */
  protected case class EPair(key: Int, pointer: Int)

  /**
   * This methods handles all the exceptions that might cause a slave actor to crash.
   * It simply informs its master actor about the crash and sends all values
   * necessary to be restarted.
   * @return
   */
  override final def exceptionHandler = {
    case e: ArrayIndexOutOfBoundsException =>
      maAc ! MsgException(e, ePair.key, ePair.pointer)
    case e: Exception =>
      maAc ! MsgException(e, ePair.key, ePair.pointer)
  }

  /**
   * This method should be used to set the values of the EPair case class.
   * @return
   */
  protected def ePair: EPair
  /**********Exception handlers - END**********/

}
