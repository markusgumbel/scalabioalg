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
import net.gumbix.dynpro.concurrency.actors.{SolutionActor, NoDepEmptyActor, MatrixActor, NoDepMatlabActor}
import ConMode._
import ConClass._
import scala.collection.mutable.ListBuffer
import scala.Array


/**
 *
 * @param clazz =: ConClass. One of the 3 possible dependency classes.
 * @param mode =: ConMode (EVENT or THREAD)
 * @param recordTime Although this isn't used here. Setting it this way makes
 *                   the attribute immutable.
 *
 * @tparam Decision
 */
protected[dynpro] final class Concurrency[Decision](
    val clazz: ConClass, mode: ConMode,
    mxRange: Int, val solRange: Int, val recordTime: Boolean) {


  /**
   * Test status: OK
   * @param n
   * @param m
   * @return
   */
  def emptyMatrix(n: Int, m: Int): Array[Array[Option[Double]]] = mode match{
    case EVENT => new NoDepEmptyActor(n, m, mxRange).getMatrix

    case THREAD => Array() //for now nothing
  }


  /**
   * This method creates a master instance (MatrixActor or MatrixThread) depending on the given
   * ConMode, which then computes the cell values of the given matrix.
   *
   * @param mx The matrix used to store all the computed values.
   * @param initVal =: initValue The value used @ the beginning of the matrix (default value).
   * @param calcCellCost The method used to compute the value of each cells.
   * @return
   */
  def computeMatrix(mx: Array[Array[Option[Double]]],
                    initVal: Double, calcCellCost:(Array[Array[Option[Double]]], Idx,
                      (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                      => Array[Array[Option[Double]]])
  : Array[Array[Option[Double]]] = mode match {
    case EVENT => new MatrixActor(mx, initVal, clazz, calcCellCost).computeMatrix

    case THREAD =>
      new MatrixThread(mx, initVal, clazz, calcCellCost).computeMatrix
  }


  /**
   *
   * @param matrix
   * @return
   */
  def convertMatrix(matrix: Array[Array[Option[Double]]]): Array[Array[Double]] = mode match {
    case EVENT =>
      new NoDepMatlabActor(matrix, mxRange).getMatrix

    case THREAD => Array() //for now do nothing
  }


  /**
   *
   * @param idx
   * @param matrix
   * @param getPathList
   * @return
   */
  def calculateSolution(idx: Idx, matrix: Array[Array[Option[Double]]],
                        getPathList:(Idx, Array[Array[Option[Double]]], (Idx, Idx)=>Boolean) => ListBuffer[PathEntry[Decision]])
  : ListBuffer[PathEntry[Decision]] = mode match{
    case EVENT =>
      new SolutionActor[Decision](idx, matrix, solRange, getPathList).getSolution

    case THREAD => new ListBuffer[PathEntry[Decision]]()//for now do nothing
  }

  /**
   * 28.05.013 -> priority 50 :)
   * @param method
   * @param emptyVal
   * @tparam DataType
   */
  private class BackUp[DataType](method: DataType, emptyVal: DataType){
    def runMethod: DataType = {
      var (keepLoopAlive, counter, toReturn) = (true, 0, emptyVal)
      while(keepLoopAlive && counter < 3){
        toReturn = method
        if(toReturn == emptyVal) counter += 1
        else keepLoopAlive = false
      }
      toReturn
    }
  }


}

