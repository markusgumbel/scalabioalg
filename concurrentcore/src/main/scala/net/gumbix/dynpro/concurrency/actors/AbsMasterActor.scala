package net.gumbix.dynpro.concurrency.actors

import scala.actors.{OutputChannel, Actor}
import net.gumbix.dynpro.concurrency.{Messages, IMaster}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 11:24 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] trait AbsMasterActor
  extends Actor with IMaster{

  //TO OVERRIDE - START
  /**
   * How to override this method.
   * - This method should exclusively contain the "react" block of the Actor.act method.
   * - It should at least contain the following cases
   *  1.  case msgException(key, loopStart) => restartSlave(stage, key, loopStart)
   *        or
   *      case msgException2(key) => restartSlave(stage, key)
   *
   *  2.   case {message inferring that the slave is done} => congestionControl ...
   */
  protected def actReact


  /**
   * This method should be used to create a block that will come
   * right after between the "startSlMods" method and the "loopWhile" loop.
   *
   * It isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   */
  protected def beforeLoopWhile{}


  /**
   * This method should be used to reply to the object, that
   * invoked/created the master actor.
  */
  protected def ackStart: Any


  /* from concurrency.IMaster
  protected def amPair: AmPair
  protected def startNewSlave(key: Int)
  protected def startNewSlave(key:Int, pos: Int) ##if necessary##
  */
  //TO OVERRIDE - END


  override final def act{
    react{
      case Messages.start => //this is an synchronous message
        val to = sender
        /**
        def afterLoopWhile{
          to ! ackStart
          exit
        }
        */
        /*
        create and start all the necessary slave actors
        This method is implemented in the IMaster trait.
        */
        startSlMods

        beforeLoopWhile
        loopWhile(keepConLoopAlive){
          actReact //This is an abstract method.
        }andThen(to ! ackStart) //(afterLoopWhile)
    }
   }


  override final def exceptionHandler = {
    case e: Exception => println(e + "\n")
  }
}
