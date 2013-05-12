package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import DependencyCase._

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
 * @param initValue The value used @ the beginning of the matrix (default value).
 * @param subMxAm =: subMatrixAmount
 * @param cActorMxL =: cellActorMatrixLength
 * @param dep =: dependencyCase One of the 3 possible dependency cases.
 * @param period =: sleepPeriod @see net.gumbix.dynpro.concurrency.Configuration
 * @param cActorAm =: cellActorAmount The amount of cell actors allowed to concurrently compute on the matrix "mx".
 * @param calcMatrixIndexValue The method used to compute the value of each cells.
 */
protected[concurrency] final class MatrixActor(
                        mx: Array[Array[Option[Double]]], initValue: Double,
                        subMxAm: Int, val cActorMxL: Int, val dep: DependencyCase,
                        val period: Int, cActorAm: Int,
                        val calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                 ) extends Actor with AbsMasterActor {

  //trapExit = true; //receive all the exceptions from the cellActors in form of messages

  override def actReact{
    react{
      case msgGetValues(missingValIndexes) =>
        val _values = for(idx <- missingValIndexes) yield mx(idx.i)(idx.j)
        /*
        The initValue can't be safely used as default value because it could occur in another cell
        as the computation result.
        Furthermore it is impossible to proceed this case without using @ least 2loops,
        hence the current structure.
          - default value =: None
          - In order to keep the overview of this case
            we have both loops directly here in this "case"
            instead of having the 1st loop here and the 2nd in the MatrixCellActorlActor.scala class.
         */
        val msg = if(_values.contains(None)) Messages.computationNotDone //'NotTotallyComputedYet
        else{
          val values = for(_v <- _values) yield _v match{ case Some(_val) => _val }
          msgAckGetValues(values)
        }
        sender ! msg

      case msgUpdateMatrix(idx, newValue) => mx(idx.i)(idx.j) = Some(newValue)

      case msgException(constantCoordinate, loopStart) =>
        restartSlave(constantCoordinate, loopStart)

      case Messages.computationDone => congestionControl

    }
  }


  override def stage = "Cell evaluation"

  override protected def amPair = AmPair(subMxAm, cActorAm)


  /**
   * This method creates and starts one MatrixCellActor.
   * @param constantCoordinate The row/column from the original matrix considered as the sub matrix that
   *                 the new MatrixCellActor will compute.
   * @param loopStart The start position in the sub matrix.
   */
  override protected def startNewSlave(constantCoordinate: Int, loopStart: Int){
    val cellActor = new MatrixCellActor(this, initValue, constantCoordinate, loopStart)
    link(cellActor)
    //start a row or column computation with the current version of the matrix.
    cellActor.start(mx)
  }

  override protected def startNewSlave(constantCoordinate: Int) = startNewSlave(constantCoordinate, 0)

  override protected def getComputedResult: Array[Array[Option[Double]]] = mx



}

