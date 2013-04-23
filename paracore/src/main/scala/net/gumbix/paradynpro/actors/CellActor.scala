package net.gumbix.paradynpro.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.paradynpro.DependencyCase._
import net.gumbix.paradynpro.{Messages, msgUpdateMatrix, msgGetValues, msgAckGetValues}

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
 * @param matrixLength The matrix length.
 * @param dependencyCase One of the 3 possible dependency cases.
 * @param calcMatrixIndexValue The method used to compute the value of each cells.
 */
protected[actors] final class CellActor(mxActor: MatrixActor,
                initValue: Double,
                matrixLength: Int, dependencyCase: DependencyCase,
                calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                                      (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                    => Array[Array[Option[Double]]]
                ) extends Actor{

  /*
  * The default value of the indexes constant coordinate. Only one of the both coordinates
  * can be constant at the time.
  */
  private var constantCoordinate = -1
  /*
  * The sub matrix. It actually has the same size as the main matrix stored in
  * the master object (MatrixActor.scala). The difference is that it's computation
  * uniquely uni directional.
  */
  private var matrix = Array(Array(Option(new Double())))
  /*
  * This attribute defines whether or not the slave is free.
  */
  private var _isFree = true


  /**
   * This method returns a suitable Index (Idx) depending on the object's dependency case.
   *
   * @param variableCoordinate
   * The current value of the indexes variable coordinate. Only one of the both coordinates
   * can be variable at the time.
   * @return An Idx object.
   */
  private def getIdx(variableCoordinate: Int): Idx = {
    dependencyCase match{
      case LEFT_UPLEFT_UP => new Idx(variableCoordinate, constantCoordinate)
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
    var missingValIndexes: List[Idx] = Nil
    var toReturnValues: List[Double] = Nil
    for(i <- 0 until prevValues.length){
      if(prevValues(i) == initValue) missingValIndexes = missingValIndexes.::(prevIndexes(i))
      else toReturnValues = toReturnValues.::(prevValues(i))
    }

    toReturnValues.:::(sendMsgGetMissingPrevValues(missingValIndexes)).toArray
  }


  /**
   * This methods sends a message to the master object (MatrixActor.scala) requesting
   * the missing values.
   *
   * @param missingValIndexes The list of the cell indexes which values are missing (None).
   * @return A List object of type Double.
   */
  private def sendMsgGetMissingPrevValues(missingValIndexes: List[Idx]): List[Double] = {
    var prevValues: List[Double] = Nil

    if(!missingValIndexes.isEmpty){
      var isComputed = false
      while(!isComputed){
        mxActor !? msgGetValues(missingValIndexes.toArray)
        prevValues = react{
          case 'NotTotallyComputedYet =>
            sleep; Nil
          case msgAckGetValues(newValues) =>
            isComputed = true; newValues.toList
        }
      }
    }

    prevValues
  }


  /**
   * Default sleep period = 10 millisecond
    */
  private def sleep = sleep(10)


  /**
   * This method forces the actor to sleep for a while.
   * @param period The period to sleep in millisecond
   */
  private def sleep(period: Int){
    val target = System.currentTimeMillis + period
    while(System.currentTimeMillis < target){}//simply stall
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


  /**
   * @see The "_free" attribute.
   * @return a Boolean value.
   */
  def isFree: Boolean = _isFree


  override def act{
    for (i <- 0 until matrixLength)
      matrix = calcMatrixIndexValue(matrix, getIdx(i), parsePrevValues, sendMsgSaveNewValue)

    //the computation of the values is done
    _isFree = true
    mxActor ! Messages.symbol(1) // notify the master (matrix actor)

    react{
      case 'Die =>
    }
  }


  /**
   * This method takes care of the preliminary settings and starts the slave (this actor).
   *
   * @param mx The current version of the matrix.
   * @param pos The constant coordinate.
   */
  def start(mx: Array[Array[Option[Double]]], pos: Int){
    matrix = mx
    constantCoordinate = pos
    _isFree = false
    super.start()
  }

}
