package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import ConClass._
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/19/13
 * Time: 4:44 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */


/**
 * This object represents the slave in this master - slave framework.
 * It starts the sub actor object and takes care of the delegated tasks.
 *
 * @param mxActor The reference to the master object.
 * @param constCoor =: constantCoordinate The value of the indexes constant coordinate.
 * @param loopStart The start position of the loop.
 */
protected[actors] final class MatrixVectorActor(mxActor: MatrixActor, constCoor: Int, loopStart: Int)
  extends AbsSlaveActor(mxActor){

  /*
  * The sub matrix. It actually has the same size as the main matrix stored in
  * the master object (MatrixActor.scala). The difference is that its computation
  * is uniquely uni directional.
  * ccp =: cost computation period
  * headStart =: the amount of ccp required to compute a dyn pro algorithm of both dep class
  * secondHeadStart =: total time required by to evaluate the 2nd cell.
  * It is impossible to use the "val" prefix for the following attributes
  * because they are supposed to be updated by 2 different blocks or even methods.
  */
  private var (matrix , loopPointer, ratio, secondHeadStart, cellCounter) =
    (Array[Array[Option[Double]]](), 0, 0, 0, 0)
  private val headStart = 5 //[ms]
  //This value has been chosen after just some few trials during the implementation stage.
  //It could therefore theoretically be improved. 30.05.2013


  /**
   * This method returns a suitable Index (Idx) depending on the object's dependency case.
   *
   * @param varCoor
   * The current value of the indexes variable coordinate. Only one of the both coordinates
   * can be variable at the time.
   * @return An Idx object.
   */
  private def getIdx(varCoor: Int): Idx = {
    mxActor.clazz match{
      case LEFT_UP => new Idx(varCoor, constCoor)
      case UP => new Idx(constCoor, varCoor)
    }
  }


  /**
   * This method collects the indexes of the cells with a missing value (None),
   * sends a message to the master object (MatrixActor.scala) requesting those missing values.
   * Once the answer has been received it merges all the values together and returns them.
   *
   * @param prevIdxs The previous indexes to the current one.
   * @param prevValues The values of the previous indexes.
   * @return An Array object of type Double.
   */
  private def getPrevValues(prevIdxs: Array[Idx], prevValues: Array[Double]): Array[Double] ={
    val (mValIdxs, toReturnValues) = (new ListBuffer[Idx](), new ListBuffer[Double]())
    for(i <- 0 until prevValues.length){
      /*
      if(prevValues(i) == mxActor.initVal) mValIdxs += prevIdxs(i)
      else toReturnValues += prevValues(i)
      */
      val idx = prevIdxs(i)
      if(matrix(idx.i)(idx.j) == None) mValIdxs += idx
      else toReturnValues += prevValues(i)
    }
    (toReturnValues ++= sendMsgGetMissingPrevValues(mValIdxs)).toArray
  }


  /**
   * This methods sends a message to the master object (MatrixActor.scala) requesting
   * the missing values.
   *
   * @param mValIdxList =: missingValIndexesList The list of the cell indexes which values are missing (None).
   * @return A List object of type Double.
   */
  private def sendMsgGetMissingPrevValues(mValIdxList: ListBuffer[Idx]): ListBuffer[Double] = {
    var prevValues = new ListBuffer[Double]()
    if(!mValIdxList.isEmpty){
      var (keepAlive, millis: Int) = (true, 0)
      //Although "loopWhile" avoid blocking the underlying thread.
      // It seems to cause a malfunction here, therefore the use of "while".
      while(keepAlive){
        prevValues = mxActor !? msgGetValues(mValIdxList) match{
          case msgAckGetValues(newValues) =>
            if(newValues.contains(None)){//the values aren't totally computed yet
              /*
              The head start is set automatically for both dependency classes
              The if statement below is the first block assuring it.
              */
              if(cellCounter == 0){
                if(millis == 0) millis = headStart * constCoor
                else millis = millis / 3
              }else if(cellCounter == 1){
                secondHeadStart += headStart
                millis = headStart
              }else if(cellCounter == 2){
                if(millis == 0) millis = mValIdxList.length * ratio
                else millis = millis / 3
              }

              Thread.sleep(millis)

              new ListBuffer[Double]()
            }else{
              keepAlive = false
              /*
              The head start is set automatically for both dependency classes
              The if statement below is the second block assuring it.
              */
              if(cellCounter == 0) cellCounter += 1
              else if(cellCounter == 1){
                //ListBuffer.length has an O(1) implementation, meanwhile ListBuffer.size has an O(n) implementation
                ratio = secondHeadStart / mValIdxList.length
                cellCounter += 1
              }

              /*
              First store the new values in the local matrix to avoid
              retrieving them in the future again.
               */
              for(z <- 0 until mValIdxList.length)
                matrix(mValIdxList(z).i)(mValIdxList(z).j) = newValues(z)

              for(nV <- newValues) yield nV.get
            }
        }
      }
    }
    prevValues
  }


  /**
   * This method sends (and forgets) a message to the master (MatrixActor.scala)
   * requesting it to store the newly computed value in the main matrix.
   *
   * @param idx The index where the value should be stored.
   * @param newValue The newly computed value.
   */
  private def sendMsgSaveNewValue(idx: Idx, newValue: Double){
    mxActor ! msgUpdateMatrix(idx, newValue)
  }


  override protected def ePair = new EPair(constCoor, loopPointer)


  override def act{
    for (i <- loopStart until mxActor.vLen){
      loopPointer = i
      matrix = mxActor.calcCellCost(matrix, getIdx(i), getPrevValues, sendMsgSaveNewValue)
    }

    //the computation of the values is done, notify the master (matrix actor) and forget (die)
    mxActor ! Messages.compDone
  }


  /**
   * This method takes care of the preliminary settings and starts the slave (this actor).
   *
   * @param mx The current version of the matrix.
   */
  def startActor(mx: Array[Array[Option[Double]]]){
    matrix = mx
    start
  }

}
