package net.gumbix.paradynpro.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.paradynpro.DependencyCase._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/19/13
 * Time: 4:44 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */



class CellActor(mxActor: MatrixActor,
                mx: Array[Array[Option[Double]]], initValue: Double,
                matrixLength: Int, dep: DependencyCase,
                calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                                      (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                    => Array[Array[Option[Double]]]
                ) extends Actor{

  private var constantCoordinate = -1
  private var matrix =  mx


  private def getIdx(variableCoordinate: Int): Idx = {
    dep match{
      case LEFT_UPLEFT_UP => new Idx(variableCoordinate, constantCoordinate)
      case UPLEFT_UP_UPRIGHT => new Idx(constantCoordinate, variableCoordinate)
    }
  }


  private def parsePrevValues(prevIndexes: Array[Idx], prevValues: Array[Double]) ={
    var missingValIndexes: List[Idx] = Nil
    for(i <- 0 until prevValues.length){
      if(prevValues(i) == initValue)
        missingValIndexes = missingValIndexes.::(prevIndexes(i))
    }

    sendMsgGetMissingPrevValues(missingValIndexes.reverse.toArray)
  }


  private def sendMsgGetMissingPrevValues(missingValIndexes: Array[Idx]): Array[Double] = {
    var prevValues: Array[Double] = Array()

    if(!missingValIndexes.isEmpty){
      var isComputed = false
      while(!isComputed){
        mxActor !? msgGetValues(missingValIndexes)
        prevValues = react{
          case symbol: Symbol =>
            sleep; Array()
          case msgAckGetValues(newValues) =>
            isComputed = true; newValues
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
   *
   * @param period The period to sleep in millisecond
   */
  private def sleep(period: Int){
    val target = System.currentTimeMillis + period
    while(System.currentTimeMillis < target){}//simply stall
  }


  /**
   * send and forget
   * @param idx
   * @param newValue
   */
  private def sendMsgSaveNewValue(idx: Idx, newValue: Double){
    mxActor ! msgUpdateMatrix(idx, newValue)
  }



  def act{
    for (i <- 0 until matrixLength)
      matrix = calcMatrixIndexValue(matrix, getIdx(i), parsePrevValues, sendMsgSaveNewValue)

    react{
      case 'Die =>
    }
  }

  def start(pos: Int){
    this.constantCoordinate = pos
    super.start()
  }

}
