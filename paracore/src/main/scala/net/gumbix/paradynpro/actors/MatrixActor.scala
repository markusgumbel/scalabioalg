package net.gumbix.paradynpro.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.paradynpro._
import net.gumbix.paradynpro.DependencyCase._

/**
 * An algorithm for parallel dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/19/13
 * Time: 4:27 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 * This object represents the master in this master - slave framework.
 * It starts the main actor object and takes care of the tasks delegation among
 * the sub actor objects (slave -> CellActor.scala).
 *
 * @param mx The matrix used to store all the computed values.
 * @param initValue The value used @ the beginning of the matrix (default value).
 * @param dependencyCase One of the 3 possible dependency cases.
 * @param cellActorAmount The amount of cell actors allowed to concurrently compute on the matrix "mx".
 * @param calcMatrixIndexValue The method used to compute the value of each cells.
 */
protected[paradynpro] final class MatrixActor(
                        mx: Array[Array[Option[Double]]], initValue: Double,
                        dependencyCase: DependencyCase, waitPeriod: Int, cellActorAmount: Int,
                        calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                 ) extends Actor with IMatrixComputer{

  trapExit = true; //receive all the exceptions from the cellActors in from of messages
  /*
  The matrix iterator distinguishes the submatrices by using a pointer "pointer".
  Each CellActor will receive a unique part of the original matrix based on the pointer's location.
  */
  private var pointer = -1

  /*
  Use the following attributes with extreme caution.
  They are set in the "startCellActors" Methods and shouldn't be set again.
   */
  private var roundsAmount= 0 // Amount of submatrices
  private var realCellActorAmount = 0 // The real amount of CellActors.
  // It depends on the size of the submatrices and the "cellActorAmount" attribute
  private var cellActorMatrixLength = 0
  private var computingCellActorAmount = 0 //amount of CellActors still alive


  /**
   * This method creates and starts one CellActor.
   * @param pointer1 The row/column from the original matrix considered as the submatrix that
   *                 the new CellActor will compute.
   * @param pointer2 The start position in the submatrix.
   */
  private def startNewCellActor(pointer1: Int, pointer2: Int){
    val cellActor = new CellActor(this, initValue, cellActorMatrixLength, dependencyCase, waitPeriod, calcMatrixIndexValue)
    link(cellActor)
    //start a row or column computation with the current version of the matrix.
    cellActor.start(mx, pointer1, pointer2)
  }
  private def startNewCellActor(pointer: Int) = startNewCellActor(pointer, 0)


  /**
   * This method takes care of starting and keeping all the slaves alive (CellActor.scala).
   */
  private def startCellActors{
    //get the suitable dimensions for the cell actors. It can only be one of the following 2 options.
    (roundsAmount, cellActorMatrixLength) = dependencyCase match {
      case LEFT_UPLEFT_UP_UPRIGHT => (mx.length, mx(0).length)
      case UPLEFT_UP_UPRIGHT => (mx(0).length, mx.length)
      /*
      * mx.length =: maximal number of cells in a column
      * mx(0).length =: maximal number of cells in a row
      */
    }

    //TODO check if my optimization is flawless
    realCellActorAmount =
      if(cellActorAmount < 1 || cellActorAmount > roundsAmount) roundsAmount
      else cellActorAmount

    var i=0
    while(i < realCellActorAmount){//the for loop doesn't work because of the last statement
      if(i > pointer){
        pointer = i //update the pointer
        computingCellActorAmount = computingCellActorAmount + 1 //update
        startNewCellActor(i)
        /*
        WARNING: do not replace "i" by "pointer", because the pointer attribute is updated concurrently
        by this and the "act" method.
         */
        //TODO think about wait
      }else i = pointer

      i = i+1
    }

  }


  override def act{
    var exitSymbol = 'empty

    loopWhile(exitSymbol != Messages.symbol(2)){
      react{
        case msgGetValues(missingValIndexes) =>
          val _values = for(idx <- missingValIndexes) yield mx(idx.i)(idx.j)
          /*
          initValue can't be safely used because the initial value could occur in another cell,
          in addition either way the loop has to be used @ least twice
          => the current structure.
           */
          val msg = if(_values.contains(None)) Messages.symbol(0) //'NotTotallyComputedYet
            else{
              val values = for(_v <- _values) yield _v match{ case Some(_val) => _val }
              msgAckGetValues(values)
            }
          sender ! msg

        case msgUpdateMatrix(idx, newValue) => mx(idx.i)(idx.j) = Some(newValue)

        case msgException(pointer1, pointer2) => startNewCellActor(pointer1, pointer2)

        case 'CellActorComputationDone =>
          pointer = pointer + 1
          val tempPointer = pointer
          /*
          WARNING: do not replace "tempPointer" by "pointer", because the pointer attribute
          is updated concurrently by this and the "startCellActors" method.
         */
          val msg = if (tempPointer < roundsAmount)
            startNewCellActor(tempPointer)
          else
            computingCellActorAmount = computingCellActorAmount - 1
            //One CellActor just died and no new one will be started at his place

          /*
          If the !mx.contains(None)-condition becomes a problem (i'm thinking about algorithms
          such as the local alignment), it can easily be removed.
          It is in fact simple security measurement. The first should work faultlessly.
           */
          if(computingCellActorAmount == 0 && !mx.contains(None)){
            exitSymbol = Messages.symbol(2)
            this ! exitSymbol //'MatrixTotallyComputed
          }

      }
    }
  }


  /**
   * @see IMatrixComputer
   */
  override def computeMatrix: Array[Array[Option[Double]]] = {
    start // start the matrix actor itself (master)
    startCellActors // create and start all the necessary cell actors (slaves)
    //wait // wait while all the values of the matrix (mx) are being computed
    react{
      //once all the values are computed, return the matrix
      case 'MatrixTotallyComputed => mx
    }
  }

}

