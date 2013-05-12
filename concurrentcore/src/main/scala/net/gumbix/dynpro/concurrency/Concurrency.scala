package net.gumbix.dynpro.concurrency

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:07 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

import net.gumbix.dynpro.{PathEntry, Idx}
import net.gumbix.dynpro.concurrency.threads.MatrixThread
import net.gumbix.dynpro.concurrency.actors.{SolutionActor, NoDepEmptyMatrixActor, MatrixActor, NoDepMatlabMatrixActor}
import ConcurrencyMode._
import DependencyCase._
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.ConcurrencyMode
import scala.Array


/**
 *
 * @param dep =: dependencyCase.
 *           One of the 3 possible dependency cases.
 * @param mode =: concurrencyMode (EVENT or THREAD)
 * @param period =: matrixSlaveSleepPeriod
 * @param matSlAm =: matrixSlavesAmount
 * @param noDepSlAm =: noDependencySlavesAmount.
 *                 The amount of cell actors allowed to concurrently compute on the matrix "mx".
 * @param solSlAm =: solutionSlavesAmount
 * @tparam Decision
 */
protected[dynpro] class Concurrency[Decision](
    val dep: DependencyCase,
    mode: ConcurrencyMode,
    period: Int,
    matSlAm: Int,
    noDepSlAm: Int,
    solSlAm: Int) {

  def this(dep: DependencyCase) =
    this(dep, ConcurrencyMode.EVENT, 10, 0, 0, 0)

  def this(dep: DependencyCase, mode: ConcurrencyMode) =
    this(dep, mode, 10, 0, 0, 0)

  def this(dep: DependencyCase, mode: ConcurrencyMode, period: Int) =
    this(dep, mode, period, 0, 0, 0)


  /**
   * This method creates a master instance (MatrixActor or MatrixThread) depending on the given
   * ConcurrencyMode, which then computes the cell values of the given matrix.
   *
   * @param mx The matrix used to store all the computed values.
   * @param initValue The value used @ the beginning of the matrix (default value).
   * @param subMatrixAmount
   * @param cellActorMatrixLength
   * @param calcMatrixIndexValue The method used to compute the value of each cells.
   * @return
   */
  def computeMatrix(mx: Array[Array[Option[Double]]],
                    initValue: Double,
                    subMatrixAmount: Int,
                    cellActorMatrixLength: Int,
                    calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                      (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                      => Array[Array[Option[Double]]])
  : Array[Array[Option[Double]]] = mode match {
    case EVENT =>
      new MatrixActor(mx, initValue, subMatrixAmount, cellActorMatrixLength,
        dep, period, matSlAm, calcMatrixIndexValue).computeMatrix
      asInstanceOf //[Array[Array[Option[Double]]]]
    case THREAD =>
      new MatrixThread(mx, initValue, subMatrixAmount, cellActorMatrixLength,
        dep, period, matSlAm, calcMatrixIndexValue).computeMatrix
    case _ => mx
  }


  /**
   *
   * @param n
   * @param m
   * @return
   */
  def emptyMatrix(n: Int, m: Int): Array[Array[Option[Double]]] = mode match{
    case EVENT =>
      new NoDepEmptyMatrixActor(n, m, noDepSlAm).computeMatrix asInstanceOf

    case THREAD => Array() //for now nothing

    case _ => Array()
  }


  /**
   *
   * @param matrix
   * @return
   */
  def convertMatrix(matrix: Array[Array[Option[Double]]]): Array[Array[Double]] = mode match {
    case EVENT =>
      new NoDepMatlabMatrixActor(matrix, noDepSlAm).computeMatrix asInstanceOf

    case THREAD => Array() //for now do nothing

    case _ => Array()
  }



  def calculateSolution(idx: Idx,
                        calculateSolution:(Idx, (Idx)=> Boolean) => ListBuffer[PathEntry[Decision]])
  : ListBuffer[PathEntry[Decision]] = mode match{
    case EVENT =>
      new SolutionActor[Decision](idx, solSlAm, calculateSolution)
        .computeMatrix asInstanceOf

    case THREAD => new ListBuffer[PathEntry[Decision]]()//for now do nothing

    case _ => new ListBuffer[PathEntry[Decision]]()
  }

}

