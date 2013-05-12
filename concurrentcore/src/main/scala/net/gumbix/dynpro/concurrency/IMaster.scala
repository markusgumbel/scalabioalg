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


  val intervalSize = 250
  private val eMap = Map[CrashPair, Int]()//key & position -> amount of crashes)

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
  protected var _sleepPeriod = 1000


  /**
   *
   * @param maxAm =: maxAllowedAmount
   * @param gAm =: givenAmount
   */
  protected case class AmPair(maxAm: Int, gAm: Int)


  private case class CrashPair(key: Int, pos: Int){
    def ==(pair: CrashPair) {key == pair.key && pos == pair.pos}
  }


  /**
   *
   * @return
   */
  protected def stage: String


  /**
   *
   * @return
   */
  protected def amPair: AmPair

  /**
   * This method should simply start the Master
   */
  protected def launchMaster

  /**
   * This method should be used to return of the matrix computation
   * @return
   */
  protected def getComputedResult: Any


  /**
   *
   * @param key
   */
  protected def startNewSlave(key: Int)

  /**
   * CAUTION:
   * This method was intentionally written this way. To be used
   * it should be overridden first. An example is provided in actors.MatrixActor.scala
   *
   * @param key
   * @param pos
   */
  protected def startNewSlave(key:Int, pos: Int) = startNewSlave(key)




  /**
   *
   * @return
   */
  protected final def keepConLoopAlive = _keepConLoopAlive


  /**
   *
   */
  protected final def startSlaves{
    val realSlAm = if(amPair.gAm < 1 || amPair.gAm > amPair.maxAm) amPair.maxAm else amPair.gAm
    for(i <- 0 until realSlAm){
      compSlCounter += 1 //update
      startNewSlave(i)
      pointer = i //update the pointer
    }
  }

  /**
   *
   * @param key
   * @param pos
   */
  protected final def restartSlave(key: Int, pos: Int){
    val pair = CrashPair(key, pos)
    if(eMap.contains(pair)){
      if(eMap(pair) == 3){ //crash protocol
      //something is really wrong -> crash all the actors including the master
        crashReport = "\nStage: " + stage + "\nLevel: " + key
        if(pos != 0) crashReport += " - Position: " + pos

        throw new Exception(crashReport)
      }else eMap + (pair -> (eMap(pair) + 1))
    }else eMap + (pair -> 1) //first crash

    startNewSlave(key, pos)
  }

  protected final def restartSlave(key: Int) = restartSlave(key, 0)


  protected final def congestionControl{
    pointer += 1
    /*
    NOTE: not to worry, "pointer" is being SEEMLY concurrently updated.
    In fact the method (independently from the moment the event related to it will be fired)
    won't be proceeded before the for - loop in the "startSlaves" method is done iterating.
    */
    if (pointer < amPair.maxAm)
      startNewSlave(pointer)
    else{//One slave just died and no new one will be started at his place
      compSlCounter -= 1
      if(compSlCounter == 0){
        _keepConLoopAlive = false
        //this ! Messages.matrixDone //'MatrixTotallyComputed
      }
    }
  }


  /**
   * The method is supposed to be implemented by the following classes
   * -MatrixActor (done)
   * -NoDepMatlabMatrixActor (in progress)
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
   * It's purpose in the "NoDepMatlabMatrixActor" and the "NoDepMatlabMatrixActor" classes
   * is simply to convert a 1D Array object of type Option[Double]
   * to a 1D Array object of type Double.
   *
   * @return
   *   if class is (MatrixActor | MatrixThread) a 2D Array object of type Option[Double]
   *   if class is (NoDepMatlabMatrixActor | MatlabMatrixThread) a 1D Array object of type Double
   *   */
  final def computeMatrix: Any = {
    launchMaster

    while(!(compSlCounter == 0 && _keepConLoopAlive == false)){//congestion control
      Thread.sleep(_sleepPeriod)
      if(_sleepPeriod > 20) _sleepPeriod /= 2
    }

    getComputedResult
  }


}
