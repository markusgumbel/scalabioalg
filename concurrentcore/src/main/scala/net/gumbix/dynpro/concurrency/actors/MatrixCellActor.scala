package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import DependencyCase._
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
 * @param initValue The value used @ the beginning of the matrix (default value).
 * @param constantCoordinate The value of the indexes constant coordinate.
 * @param loopStart The start position of the loop.
 */
protected[actors] final class MatrixCellActor(mxActor: MatrixActor,
                initValue: Double, constantCoordinate: Int, loopStart: Int) extends AbsSlaveActor(mxActor){

  /*
  * The sub matrix. It actually has the same size as the main matrix stored in
  * the master object (MatrixActor.scala). The difference is that it's computation
  * uniquely uni directional.
  * It is impossible to use the "val" prefix because they are supposed to be updated
  * by 2different methods.
  */
  private var matrix = Array[Array[Option[Double]]]()

  private var loopPointer = 0


  /**
   * This method returns a suitable Index (Idx) depending on the object's dependency case.
   *
   * @param variableCoordinate
   * The current value of the indexes variable coordinate. Only one of the both coordinates
   * can be variable at the time.
   * @return An Idx object.
   */
  private def getIdx(variableCoordinate: Int): Idx = {
    mxActor.dep match{
      case LEFT_UPLEFT_UP_UPRIGHT => new Idx(variableCoordinate, constantCoordinate)
      case UPLEFT_UP_UPRIGHT => new Idx(constantCoordinate, variableCoordinate)
    }
  }


  /**
   * This method collects the indexes of the cells with a missing value (None),
   * sends a message to the master object (MatrixActor.scala) requesting those missing values.
   * Once the answer has been received it merges all the values together and returns them.
   *
   * @param prevIndexes The previous indexes to the current one.
   * @param prevValues The values of the previous indexes.
   * @return An Array object of type Double.
   */
  private def parsePrevValues(prevIndexes: Array[Idx], prevValues: Array[Double]): Array[Double] ={
    var missingValIndexes = new ListBuffer[Idx]()
    var toReturnValues = new ListBuffer[Double]()
    for(i <- 0 until prevValues.length){
      if(prevValues(i) == initValue) missingValIndexes += prevIndexes(i)
      else toReturnValues += prevValues(i)
    }

    (toReturnValues ++= sendMsgGetMissingPrevValues(missingValIndexes)).toArray
  }


  /**
   * This methods sends a message to the master object (MatrixActor.scala) requesting
   * the missing values.
   *
   * @param missingValIndexes The list of the cell indexes which values are missing (None).
   * @return A List object of type Double.
   */
  private def sendMsgGetMissingPrevValues(missingValIndexes: ListBuffer[Idx]): ListBuffer[Double] = {
    var prevValues: ListBuffer[Double] = new ListBuffer[Double]()
    if(!missingValIndexes.isEmpty){
      var isComputed = false
      //I chose "loopWhile" over "while" to avoid blocking the underlying thread
      loopWhile(!isComputed){
        prevValues = mxActor !? msgGetValues(missingValIndexes) match{
          case Messages.computationNotDone => Thread.sleep(mxActor.period); new ListBuffer[Double]()
          case msgAckGetValues(newValues) => isComputed = true; newValues
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


  override def ePair = new EPair(constantCoordinate, loopPointer)


  override def act{
    for (i <- loopStart until mxActor.cActorMxL){
      loopPointer = i
      matrix = mxActor.calcMatrixIndexValue(matrix, getIdx(i), parsePrevValues, sendMsgSaveNewValue)
    }
    //the computation of the values is done, notify the master (matrix actor) and forget (die)
    mxActor ! Messages.computationDone
  }


  /**
   * This method takes care of the preliminary settings and starts the slave (this actor).
   *
   * @param mx The current version of the matrix.
   */
  def start(mx: Array[Array[Option[Double]]]){
    matrix = mx
    start
  }

}
