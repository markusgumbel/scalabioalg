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
                        dependencyCase: DependencyCase, cellActorAmount: Int,
                        calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                 ) extends Actor with IMatrixComputer{


  /**
   * This method takes care of starting and keeping all the slaves alive (CellActor.scala).
   */
  private def startCellActors{
    //get the suitable dimensions for the cell actors
    val (roundsAmount, cellActorMatrixLength) = dependencyCase match {
      case LEFT_UPLEFT_UP => (mx.length, mx(0).length)
      case UPLEFT_UP_UPRIGHT => (mx(0).length, mx.length)
      case _ => (0,0)
      /*
      * mx.length =: maximal number of cells in a column
      * mx(0).length =: maximal number of cells in a row
      */
    }

    //start all the cell actors
    var cellActor: CellActor = null //default slave (CellActor.scala)
    var cellActorMap: Map[Int, CellActor] = Map() //empty map

    //TODO check if my optimization is flawless
    /*
    * Once the amount of rounds found, the loop creates, if necessary
    * (depending on the allowed amount) keeps the slaves alive (CellActor.scala)
    * and start them.
    */
    for(i <- 0 until roundsAmount){
      if (cellActorAmount < 1 || i < cellActorAmount){
        cellActor = new CellActor(this, initValue, cellActorMatrixLength, dependencyCase, calcMatrixIndexValue)
        cellActorMap += (i -> cellActor)
      }else{
        var cellActorIsFound = false
        var j = 0
        // the loop will run until a free cell actor is found.
        while(!cellActorIsFound) if(j == cellActorMap.size){
          j = -1
        }else if(cellActorMap.get(j).get.isFree){
          cellActor = cellActorMap.get(j).get
          cellActorIsFound = true
        }else j = j+1
      }

      //start a row or column computation with the current version of the matrix.
      cellActor.start(mx, i)
    }

  }


  override def act{
    react{
      case msgGetValues(missingValIndexes) =>
        val _values = for(idx <- missingValIndexes) yield mx(idx.i)(idx.j)
        /*
        initValue can't be safely used because the initial value could occur in another cell,
        in addition either way the loop has to be used @ least twice
        => the current structure.
         */
        val msg = if(_values.contains(None)) Messages.symbol(0)
          else{
            val values = for(_v <- _values) yield _v match{ case Some(_val) => _val }
            msgAckGetValues(values)
          }
        sender ! msg

      case msgUpdateMatrix(idx, newValue) =>
        mx(idx.i)(idx.j) = Some(newValue)

      case 'CellActorComputationDone =>

      case 'Die =>
    }
  }


  /**
   * @see IMatrixComputer
   */
  override def computeMatrix: Array[Array[Option[Double]]] = {
    startCellActors // create and start all the necessary cell actors (slaves)
    start // start the matrix actor itself (master)
    wait // wait while all the values of the matrix (mx) are being computed
    react{
      //once all the values are computed, return the matrix
      case 'MatrixTotallyComputed => mx
    }
  }

}

