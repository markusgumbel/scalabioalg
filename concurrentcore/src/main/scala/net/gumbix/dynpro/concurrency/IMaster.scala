package net.gumbix.dynpro.concurrency

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


  //protected[concurrency] val range = 200

  /* It is impossible to use the "val" prefix for the following attributes
  * because they are supposed to be updated by @ least 2 different methods. */
  /*
  The matrix iterator distinguishes the sub matrices by using a pointer "pointer".
  Each slave will receive a unique part of the original matrix based on the pointer's location.
  */
  private var
  (pointer, crashReport, _keepConLoopAlive, compSlCounter) = (-1, "", true, 0)
   //amount of slaves still alive

  //this is the default value, OVERRIDE NECESSARY
  protected var sleepPeriod = 1000


  /*Exception handlers - START*/
  /*
  This map is used to store and keep an overview on the crashes
  of the slave modules.
  key & position -> amount of crashes
  */
  private var eMap = Map[EPair, Int]()
  /**
   * This case class is used as key in the map above.
   * @param key level for the solution stage, row for the rest.
   * @param pos cell within the row during the matrix stage.
   */
  private case class EPair(key: Int, pos: Int){
    def ==(pair: EPair) {key == pair.key && pos == pair.pos}
  }
  /**
   * This case class is represents the term instance that will be used for the
   * printed version of the crash report.
   * @param stage one of the four stages.
   * @param key level for the solution stage, row for the rest.
   * @param pointer cell within the row during the matrix stage.
   */
  protected case class ETerms(stage: String, key: String, pointer: String)

  /**
   * This method is used to set the ETerm instance.
   * @return
   */
  protected def eTerms: ETerms
  /*Exception handlers - END*/


  /*Pool size - START*/
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

  private val slPoolSize = getPoolSize.slMod
  /*Pool size - END*/


  /*Abstract methods - START*/
  /**
   * This method should simply start the Master
   */
  protected def launchMaster

  /**
   *
   * @param key
   */
  protected def startNewSlMod(key: Int)

  /**
   * This isn't an abstract method but should be considered as one.
   * This way in contrast to the "actReact" method it will be
   * overridden if only there's a need.
   *
   * @param key
   * @param pos
   */
  protected def startNewSlMod(key:Int, pos: Int){startNewSlMod(key)}
  /*Abstract methods - END*/


  /**
   *
   * @return
   */
  protected final def keepConLoopAlive = _keepConLoopAlive


  /**
   *
   * @param key
   * @param pos
   */
  protected final def restartSlMod(key: Int, pos: Int){
    val pair = EPair(key, pos)
    if(eMap.contains(pair)){
      if(eMap(pair) == 3){ //crash protocol
      //something is really wrong -> crash all the actors including the master
        val terms = eTerms
        crashReport = "\nStage: " + terms.stage + "\n"+ terms.key +": " + key
        if(!eTerms.pointer.isEmpty) crashReport += " - "+ terms.pointer +": " + pos

        throw new Exception(crashReport)
      }else eMap += pair -> (eMap(pair) + 1)
    }else eMap += pair -> 1 //first crash

    startNewSlMod(key, pos)
  }

  protected final def restartSlMod(key: Int){restartSlMod(key, 0)}


  /**
   *
   */
  protected final def startSlMods{
    /*The pool size value 100E6 is an experimental value
    * To get the value appropriate to your os run: Debugger.getMaxPoolSize*/

    val maxPoolSize = Runtime.getRuntime().availableProcessors() * 100E6 - getPoolSize.subSlMod
    val realPoolSize = if (slPoolSize > maxPoolSize) maxPoolSize else getPoolSize.slMod
    //println(this + "--->" + maxPoolSize)
    for(i <- 0 until realPoolSize.asInstanceOf[Int]){
      compSlCounter += 1 //update
      startNewSlMod(i)
      pointer = i //update the pointer
    }
  }


  protected final def congestionControl{
    pointer += 1
    /*
    NOTE: not to worry, "pointer" is being SEEMLY concurrently updated.
    In fact the method (independently from the moment the event related to it will be fired)
    won't be proceeded before the for - loop in the "startSlaves" method is done iterating.
    */
    if (pointer < slPoolSize)
      startNewSlMod(pointer)
    else{//One slave just died and no new one will be started at his place
      compSlCounter -= 1
      if(compSlCounter == 0)  _keepConLoopAlive = false
        //this ! Messages.matrixDone //'MatrixTotallyComputed
    }
  }


  /**
   * The method is supposed to be implemented by the following classes
   * -MatrixActor (done)
   * -NoDepMatlabActor (in progress)
   * -MatrixThread (not implemented yet)
   * -MatlabMatrixThread (not implemented yet)
   *
   * Its purpose in the "MatrixActor" and the "MatrixThread" classes
   * is to perform in the given order the following tasks:
   * 1- start all the slaves (cell threads or - actors),
   * 2- start the ONLY master (matrix thread or - actor),
   * 3- wait until the matrix is fully computed,
   * 4- return it.
   *
   * It's purpose in the "NoDepMatlabActor" and the "NoDepMatlabActor" classes
   * is simply to convert a 1D Array object of type Option[Double]
   * to a 1D Array object of type Double.
   *
   * @return
   *   if class is (MatrixActor | MatrixThread) a 2D Array object of type Option[Double]
   *   if class is (NoDepMatlabActor | MatlabMatrixThread) a 1D Array object of type Double
   *   */
  final def preCompute{
    launchMaster

    while(!(compSlCounter == 0 && _keepConLoopAlive == false)){
      Thread.sleep(sleepPeriod)
      if(sleepPeriod > 50) sleepPeriod *= (2/3)//congestion control
    }

    //-> pre compute is done. now return results in the next def
  }
}
