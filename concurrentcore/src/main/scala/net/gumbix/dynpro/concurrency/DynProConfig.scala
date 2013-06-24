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
import net.gumbix.dynpro.concurrency.actors.
{NoDepEmptyActor, MxLeftUpActor, MxUpActor, NoDepMatlabActor, SolutionActor}
import ConMode._
import ConClass._
import scala.collection.mutable.{Map, ListBuffer}
import scala.Array
import Module._
import Stage._
import scala.actors.Actor


/**
 *
 * @param clazz =: ConClass. One of the 3 possible dependency classes.
 * @param mode =: ConMode (EVENT or THREAD)
 * @param mxRange
 * @param bcSize
 * @param solRange
 * @param recordTime Although this isn't used here. Setting it this way makes
 *                   the attribute immutable.
 *
 * @tparam Decision
 */
protected[dynpro] final class DynProConfig[Decision](
    val clazz: ConClass, mode: ConMode,
    mxRange: Int, bcSize: Int, val solRange: Int, val recordTime: Boolean) {

  private val maModules: Map[Stage, Actor] = Map()

  private def getActor(stage: Stage, newVal: Actor) = try{
    //maModules(stage).update
    maModules(stage).restart
    maModules(stage)
  }catch{ case e: NoSuchElementException =>
    newVal.start
    maModules(stage) = newVal
    newVal
  }



  /**
   * Test status: OK
   * @param n
   * @param m
   * @return
   */
  def emptyMatrix(n: Int, m: Int): Array[Array[Option[Double]]] = mode match{
    case EVENT =>
      getActor(empty, new NoDepEmptyActor(n, m, mxRange)) !? Messages.start match{
        case matrix: Array[Array[Option[Double]]] => matrix
      }

    case THREAD => Array() //for now nothing
  }


  /**
   * This method creates a master instance (MatrixActor or MatrixThread) depending on the given
   * ConMode, which then computes the cell values of the given matrix.
   *
   * @param _matrix The matrix used to store all the computed values.
   * @param getAccValues The first method used to compute the value of each cells.
   * @param calcNewAccValue The second method used to compute the value of each cells.
   * @return
   */
  def computeMatrix(_matrix: Array[Array[Option[Double]]],
  getAccValues:(Array[Array[Option[Double]]], Idx, Idx => Unit) => Array[Double],
  calcNewAccValue:(Array[Double]) => Option[Double])
  : Array[Array[Option[Double]]] = mode match{
    case EVENT => //print("Matrix " + Runtime.getRuntime.freeMemory + " => ")
      def getMatrix() = _matrix
      val actor = clazz match{
        case LEFT_UP =>
          getActor(matrix, new MxLeftUpActor(getMatrix, bcSize, getAccValues, calcNewAccValue))
        case UP =>
          getActor(matrix, new MxUpActor(getMatrix, bcSize, getAccValues, calcNewAccValue))
      }

      actor !? Messages.start match{
        case matrix: Array[Array[Option[Double]]] =>  matrix
          //println(" => return " + actor.getState)
      }

    case THREAD => Array()
  }


  /**
   *
   * @param matrix
   * @return
   */
  def convertMatrix(matrix: Array[Array[Option[Double]]]): Array[Array[Double]] = mode match {
    case EVENT =>
      def getMatrix() = matrix
      getActor(matlabMx, new NoDepMatlabActor(getMatrix, mxRange)) !? Messages.start match{
        case matrix: Array[Array[Double]] => matrix
      }

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
      def getMatrix() = matrix
      def getIdx() = idx
      getActor(solution, new SolutionActor[Decision](getIdx, getMatrix, solRange, getPathList)) !? Messages.start match{
        case pathList: ListBuffer[PathEntry[Decision]] => pathList
      }

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

