package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency.{msgException, IMaster}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 7:22 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] abstract class AbsSlaveActor(master: AbsMasterActor)extends Actor{

  /*
  This way all NoDepRowActor objects will crash if their
  NoDepAbsActor crashes. That way we save resources.
  */
  link(master)

  //internal actors and counter
  /*
  The counter is used to constantly have an overview on the current amount of
  internal actors.
  Once their work done there is no further need to keep the "loop" (irritating them)
  alive.
  */
  private var counter = -1
  protected def getCounter = counter
  protected def add = counter += 1
  protected def subtract = counter -= 1
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


  //exception
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
  override def exceptionHandler = {
    case e: Exception => master ! msgException(ePair.key, ePair.pointer)
  }

  /**
   * This method should be used to set the values of the EPair case class.
   * @return
   */
  protected def ePair: EPair


}
