package net.gumbix.dynpro.concurrency

import net.gumbix.dynpro.{PathEntry, Idx}
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 12:13 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[gumbix] object Messages {
//symbol messages
  val START = 'start
  protected[concurrency] val WAKEUP = 'wakeUp //only required in case of dependency, hence the visibility scope.
  val DONE = 'done
}

//Used in all master actors
protected[concurrency] case class MsgException(e: Exception, key: Int, pointer: Int)

