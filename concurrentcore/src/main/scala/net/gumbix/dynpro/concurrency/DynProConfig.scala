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
 *
 * The parameters are set in their order of importance.
 * @param clazz =: ConClass. One of the 3 possible dependency classes.
 * @param mode =: ConMode (EVENT or THREAD)
 * @param solRange The purpose of this param is to avoid StackOverflowErrors while computing
 *                 with very long sequences. It is used to set an adequate sequence length during
 *                 the path finding stage (stage II).
 * @param bcMailSize wake up frequency =: the number of costs that have to be calculated
 *               before the next wake up broadcast is made.
 * @param mxRange This param has the same purpose as the "solRange" param. It is used during the invocation
 *                of the "convertMatrix" method.
 * @tparam Decision
 */
protected[dynpro] final class DynProConfig[Decision](
  val clazz: ConClass, mode: ConMode, val solRange: Int, bcMailSize: Int, mxRange: Int
){

  /**
   * This method creates a master instance (MatrixActor or MatrixThread) depending on the given
   * ConMode, which then computes the cell values of the given matrix.
   * @param n self explanatory
   * @param m self explanatory
   * @param getAccValues The first method used to compute the value of each cells.
   * @param calcCellCost The second method used to compute the value of each cells.
   * @return
   */
  def evaluateMatrix(
    n: Int, m: Int,
    getAccValues:(Idx, Idx => Unit) => Array[Double],
    calcCellCost:(Idx, Array[Double]) => Unit
  ): Boolean = {
    val actor = clazz match{
      case LEFT_UP => new MxLUpActor(n, m, bcMailSize, getAccValues, calcCellCost)
      case UP => new MxUpActor(m, n, bcMailSize, getAccValues, calcCellCost)
    }

    mode match{
      case EVENT =>
        actor.start
        actor !? START match{case DONE => true} //the matrix evaluation is done

      case THREAD => false //do nothing for now
    }
  }


  /**
   *
   * @param n self explanatory
   * @param m self explanatory
   * @param convert The method used to convert the values of the cells
   * @return
   */
  def convertMatrix(n: Int, m: Int, convert:(Idx) => Unit) = mode match {
    case EVENT =>
      val actor = new MatlabActor(n, m, convert, mxRange)
      actor.start
      actor !? START match{case DONE => true} //the matrix convertion is done

    case THREAD => false //do nothing for now
  }


  /**
   * This method is used to concurrently get the solution path.
   * @param idx The index from where the back tracking will start.
   * @param getPath This method is used to get the solution path
   * @return
   */
  def calculateSolution(
    idx: Idx,
    getPath:(Idx, (Idx, Idx)=>Boolean) => ListBuffer[PathEntry[Decision]]
  ): ListBuffer[PathEntry[Decision]] = mode match{
    case EVENT =>
      val actor = new SolutionActor[Decision](idx, getPath, solRange)
      actor.start
      actor !? START match{case pathList: ListBuffer[PathEntry[Decision]] => pathList}
      //the path has been found.

    case THREAD => new ListBuffer[PathEntry[Decision]]()//for now do nothing
  }

}

