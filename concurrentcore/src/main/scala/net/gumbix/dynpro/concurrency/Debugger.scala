package net.gumbix.dynpro.concurrency

import scala.actors.Actor
import scala.actors.Actor._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/13/13
 * Time: 10:26 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 * This object is solely for test purposes.
 */
private class StressActor extends Actor{

  class SubActor extends Actor{
    override def act{
      react{
        case 'pause => println("I shouldn't be here \n" + this)
      }
    }
  }

  val (quot, core) = (1E5, Runtime.getRuntime().availableProcessors)
  var (counter, toPrint) = (1, "")


  override def act{
    var i: Long = 0
    loopWhile(true){
      if(i % quot == 0){
        counter += 1
        toPrint = counter + " * " + quot
        println("-> " + toPrint)
        i = 0
      }
      val a = new SubActor
      a.start
      i += 1
    }
  }

  override def exceptionHandler = {
    case e: Exception =>
      toPrint = "\nThe limit is = " + toPrint + " for " + core + " core(s)"
      println(e + toPrint)
  }
}

object Debugger {

  def print(text: String){
    println(text)
    System.exit(0)
  }

  def print(any: Any){print(any.toString)}

  def print(){print("here i am!!!")}

  def getMaxPoolSize{
    println("Los geht's")
    new StressActor().start
  }

  def main(args: Array[String]) {
    getMaxPoolSize
  }

}
