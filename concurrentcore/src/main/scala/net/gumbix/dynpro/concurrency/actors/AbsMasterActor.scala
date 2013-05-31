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
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
  */
  protected def loopWhileAndThen{}

  /*from concurrency.IMaster
  protected def amPair: AmPair
  protected def startNewSlave(key: Int)
  protected def startNewSlave(key:Int, pos: Int) ##if necessary##
   */
  //TO OVERRIDE - END


  override final protected def launchMaster = start


  override final def act{
    /*
    create and start all the necessary slave actors
    This method is implemented in the IMaster trait.
    */
    startSlMods

    loopWhile(keepConLoopAlive){
      actReact //This is an abstract method.
    }andThen(loopWhileAndThen)

  }


  override final def exceptionHandler = {
    case e: Exception => println(e + "\n\n")
  }
}
