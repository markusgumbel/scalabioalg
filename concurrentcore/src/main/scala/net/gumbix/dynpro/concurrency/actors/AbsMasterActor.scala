package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency.IMaster

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 11:24 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class is a part of each master actor in this prototype.
 * It simplifies the implementation of each specific slave actor
 * by increasing the abstraction level set in IMatrix.scala and adapting
 * it to the "actor" API.
 */
protected[actors] abstract class AbsMasterActor extends IMaster with Actor{

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
   * This method should be used to insert a block that will come
   * right after between the "startSlMods" method and the "loopWhile" loop.
   * So far it is overridden by the MyUpActor class.
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
  protected def iniNewSlMod(key: Int)
  protected def restartSlMod(key:Int) ##if necessary##
  */
  //TO OVERRIDE - END

  //OVERRIDDEN - START
  override final def act{
    react{
      case net.gumbix.dynpro.concurrency.Messages.START => //this is a synchronous message
        //due to the 2nd react the value of the "sender" attribute is internally updated
        //that's why the initial value has to be copied to be preserved
        val to = sender

        /* Create and start all the necessary slave actors.
         * This method is implemented in the IMaster trait.
         */
        iniSlMods
        beforeLoopWhile

        loopWhile(keepConLoopAlive){
          actReact //This is an abstract method.
        }andThen to ! ackStart //afterLoopWhile
    }
   }


  override final def exceptionHandler = {
    case e: Exception => System.err.println(e + "\n"); System.exit(-1)
  }
  //OVERRIDDEN - END

}
