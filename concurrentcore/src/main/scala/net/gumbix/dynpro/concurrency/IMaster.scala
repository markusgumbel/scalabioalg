package net.gumbix.dynpro.concurrency

import net.gumbix.dynpro.Idx
import scala.collection.mutable.ListBuffer
import scala.actors.Actor
import net.gumbix.dynpro.concurrency.actors.MxLUpActor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/10/13
 * Time: 11:28 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 * This trail can and should be used be all Masters (event- based and thread-based)
 * in the package "concurrency".
 */
protected[concurrency] trait IMaster{

  /*
  It is impossible to use the "val" prefix for the following attributes
  because they are supposed to be updated by @ least 2 different methods.
  --------------------
  The matrix iterator distinguishes the sub matrices by using a pointer "pointer".
  Each slave will receive a unique part of the original matrix based on the pointer's location.
  */
  private var (pointer, _keepConLoopAlive, compSlCounter) = (-1, true, 0)

  //all the slave modules
  protected val slModules = ListBuffer[Actor]()

  /**********Exception handlers - START**********/
  /**
   * This case class represents the term instance that will be used to
   * print out of the crash report.
   * @param stage one of the four stages.
   * @param key level (for the solution stage) or
   *            column(for the "UP" dependency class) or
   *            row (for the rest).
   * @param pointer cell within the row/column vector during the matrix stage.
   */
  protected case class ETerms(stage: String, key: String, pointer: String)

  /**
   * This method is used to set the ETerm instance.
   * @return
   */
  protected def eTerms: ETerms
  /**********Exception handlers - END**********/


  /**********Pool size - START**********/
  /**
   *
   * @param slMod = slave module
   * The amount of slave modules allowed to compute concurrently
   * work on the matrix "mx" for a given computation stage.
   * @param subSlMod = sub slave module =: a slave module to a prior slave module
   * This parameter is used to set the amount of resources
   * that should be reserved for the sub slave module.
   */
  protected case class PoolSize(slMod: Int, subSlMod: Int)

  protected def getPoolSize: PoolSize

  protected val dMaxPoolSize = Runtime.getRuntime().availableProcessors() * 100E6
  /**********Pool size - END**********/


  /**********Abstract methods - START**********/
  /**
   *
   * @param key
   */
  protected def startNewSlMod(key: Int)

  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   * @param key
   */
  protected def restartSlMod(key:Int)

  /**
   *
   * @return
   */
  //protected[concurrency] def getMatrix: Array[Array[Option[Double]]]
  /**********Abstract methods - END**********/


  /**********Final protected methods - START**********/
  /**
   * This method is used to return the current value of the
   * "_keepConLoopAlive" attribute.
   * @return
   */
  protected final def keepConLoopAlive = _keepConLoopAlive
  protected final def setLoopCond{
    _keepConLoopAlive = true
    pointer = -1
  }

  /**
   * This method is invoked if an exception is fired by a slave module.
   * @param e Exception
   * @param key level (for the solution stage) or
   *            column(for the "UP" dependency class) or
   *            row (for the rest).
   * @param pos cell within the row/column vector during the matrix stage.
   */
  protected final def handleException(e: Exception, key: Int, pos: Int){
    val terms = eTerms
    val sp = " - "
    var crashReport = e + "\nStage: " + terms.stage + sp + terms.key +": " + key
    if(!eTerms.pointer.isEmpty) crashReport += sp + terms.pointer +": " + pos

    throw new Exception(crashReport)
    //Debugger.print(crashReport) //print and kill all the other modules
  }


	private def runStartSlModsLoopIt(activateActor: Int => Unit, i: Int){
		compSlCounter += 1 //update
		activateActor(i)
		//pointer = i //update the pointer
	}
  /**
   * This method is used to start the first wave of slave modules.
   */
  protected final def startSlMods{
    /*The pool size value 100E6 is an experimental value
    * To get the value appropriate to your os run: Debugger.getMaxPoolSize*/
    val (realPoolSize, len) =
      (math.min(dMaxPoolSize - getPoolSize.subSlMod, getPoolSize.slMod),
       slModules.length)

    for(i <- 0 until len){//the first wave
      if(i == realPoolSize.asInstanceOf[Int]){
        pointer = realPoolSize.asInstanceOf[Int]
        return
      }
      runStartSlModsLoopIt(restartSlMod, i)
    }

    for(i <- len until realPoolSize.asInstanceOf[Int])
      runStartSlModsLoopIt(startNewSlMod, i) //the second wave

    pointer = realPoolSize.asInstanceOf[Int]
  }


  /**
   * This method is used to keep an overview on the slave modules and
   * if required start slave modules after the first wave.
   */
  protected final def congestionControl{
    /*
    NOTE: not to worry, "pointer" is being SEEMLY concurrently updated.
    In fact the method (independently from the moment the event related to it will be fired)
    won't be proceeded before the for - loop in the "startSlaves" method is done iterating.
    */

    /* pointer += 1
    if(pointer < slModules.length) restartSlMod(pointer)
    else if(pointer < getPoolSize.slMod) startNewSlMod(pointer)*/

    if(pointer < getPoolSize.slMod){
      if(pointer < slModules.length) restartSlMod(pointer)
      else startNewSlMod(pointer)
      pointer += 1
    }else{//One slave just finished and no new one will be (re)started at his place
      compSlCounter -= 1
      if(compSlCounter == 0) _keepConLoopAlive = false
    }
  }
  /**********Final protected methods - END**********/
}