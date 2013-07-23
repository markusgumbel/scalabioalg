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
import actors.{MxLUpActor, MxUpActor, MatlabActor, SolutionActor}
import ConClass._
import ConMode._
import Stage._
import Messages._
import scala.collection.mutable.{Map, ListBuffer}
import scala.actors.Actor

/**
 * This object takes care of running the concurrency by create an abstraction level on
 * 2 essential aspects.:
 * - The actors aren't accessible/visible from outside of this package.
 *   Making a remodelling of this package much easier.
 * - The matrix object itself isn't directly accessible to the actors.
 *   Reducing the effort and time during its relocation or outsourcing (in a cloud environment for e.g.).
 * @param clazz =: ConClass. One of the 3 possible dependency classes.
 * @param mode =: ConMode (EVENT or THREAD)
 * @param getDim
 * @param getIdx
 * @param getAccValues The first method used to compute the value of each cells.
 * @param calcCellCost The second method used to compute the value of each cells.
 * @param getPath
 * @param mxRange
 * @param wuFreq wake up frequency =: the number of costs that have to be calculated
 *               before the next wake up broadcast is made.
 * @param solRange
 * @param recordTime Although this isn't used here. Setting it this way makes
 *                   the attribute immutable.
 *
 * @tparam Decision
 */
protected[dynpro] final class DynProConfig[Decision](
  val clazz: ConClass, mode: ConMode, val recordTime: Boolean,
  wuFreq: Int, getDim:() => (Int, Int),
  getAccValues:(Idx, Idx => Unit) => Array[Double],
  calcCellCost:(Idx, Array[Double]) => Unit,
  mxRange: Int, convert:(Idx) => Unit,
  getIdx: => Idx,
  getPath:(Idx, (Idx, Idx)=>Boolean) => ListBuffer[PathEntry[Decision]],
  val solRange: Int
){

  private val maModules: Map[Stage, Actor] = Map(
    MATRIX -> (clazz match{//the alternative would be to create to create singelton classes
      case LEFT_UP => new MxLUpActor(getDim, wuFreq, getAccValues, calcCellCost)
      case UP => new MxUpActor(getDim, wuFreq, getAccValues, calcCellCost)
      case _ => null
    }),
    MATLABMX -> new MatlabActor(getDim, convert, mxRange),
    SOLUTION -> new SolutionActor[Decision](getIdx, getPath, solRange)
  )

  /**
   * This method (re)STARTs the actor whose key corresponds
   * to the given "stage" value, before returning it.
   * @param stage
   * @return
   */
  private def getActor(stage: Stage) = {
    val actor = maModules(stage)
    try{actor.restart}
    catch{case e: IllegalStateException => actor.start}
    actor
  }

  /**
   * This method creates a master instance (MatrixActor or MatrixThread) depending on the given
   * ConMode, which then computes the cell values of the given matrix.
   * @return
   */
  def evaluateMatrix{
    mode match{
      case EVENT =>
        getActor(MATRIX) !? START match{
          case DONE => //the matrix evaluation is done
        }

      case THREAD => //do nothing for now
    }
  }

  /**
   *
   * @return
   */
  def convertMatrix{
    mode match {
      case EVENT =>
        getActor(MATLABMX) !? START match{
          case DONE => //the matrix convertion is done
        }

      case THREAD => //do nothing for now
    }
  }

  /**
   *
   * @return
   */
  def calculateSolution: ListBuffer[PathEntry[Decision]] = mode match{
    case EVENT =>
      getActor(SOLUTION) !? START match{
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

