package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import ConClass._

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
 * the sub actor objects (slave -> MatrixCellActorlActor.scala).
 *
 * @param mx The matrix used to store all the computed values.
 * @param initVal The value used @ the beginning of the matrix (default value).
 * @param clazz =: dependencyCase One of the 3 possible dependency cases.
 * @param calcCellCost The method used to compute the value of each cells.
 */
protected[concurrency] final class MatrixActor(
                        mx: Array[Array[Option[Double]]], val initVal: Double,
                        val clazz: ConClass,
                        val calcCellCost:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                 ) extends Actor with AbsMasterActor{

  //trapExit = true; //receive all the exceptions from the cellActors in form of messages

  /*
   * vAm =: vector amount
   * vLen =: vector length (used in MatrixVectorActor)
   */
  protected[actors] val (vAm, vLen) = clazz match{
   case UP => (mx.length, mx(0).length)
   case LEFT_UP => (mx(0).length, mx.length)
  }


  override protected def actReact{
    react{
      case msgGetValues(mValIdxList) =>
        //val values = for(idx <- missingValIndexes) yield mx(idx.i)(idx.j)
        sender ! msgAckGetValues(for(idx <- mValIdxList) yield mx(idx.i)(idx.j))

      case msgUpdateMatrix(idx, newValue) => mx(idx.i)(idx.j) = Some(newValue)

      case msgException(constantCoordinate, loopStart) =>
        restartSlMod(constantCoordinate, loopStart)

      case Messages.compDone => congestionControl
    }
  }


  override protected def eTerms = {
    val key = clazz match{
      case LEFT_UP => "Row"
      case UP => "Column"
    }
    ETerms("Cell evaluation", key, "Cell")
  }

  /**
   * Slave module =: vector actor
   * @return
   */
  override protected def getPoolSize = PoolSize(vAm, 0)


  /**
   * This method creates and starts one MatrixVectorActor.
   * @param constCoord =: constantCoordinate The row/column from the original matrix considered as the sub matrix that
   *                 the new MatrixVectorActor will compute.
   * @param loopStart The start position in the sub matrix.
   */
  override protected def startNewSlMod(constCoord: Int, loopStart: Int){
    //start a row or column computation with the current version of the matrix.
      new MatrixVectorActor(this, constCoord, loopStart)
        .startActor(mx)
  }

  override protected def startNewSlMod(constantCoordinate: Int) = startNewSlMod(constantCoordinate, 0)


  def computeMatrix: Array[Array[Option[Double]]] = {
    preCompute
    mx
  }



}

