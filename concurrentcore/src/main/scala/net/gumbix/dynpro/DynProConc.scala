package net.gumbix.dynpro

import concurrency.{Concurrency, ConcurrencyMode, DependencyCase}
import ConcurrencyMode.EVENT
import DependencyCase._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/15/13
 * Time: 3:42 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */


abstract class DynProConc[Decision] extends net.gumbix.dynpro.DynPro {

  //default configuration, OVERRIDE IF NECESSARY
  override val config = Configuration(LEFT_UPLEFT_UP_UPRIGHT, new Concurrency[Decision](EVENT), 0, 0, 10)
  //n = mx.length
  //m = mx(0).length


  /**
   * @see net.gumbix.dynpro.DynProConc.matrix
   */
  override lazy val matrix: Array[Array[Option[Double]]] = {
    /* Iterate through all the cells. Note that the order depends on the
    problem. Many versions go from top to bottom and left to right.
    However, any other order may also work. */

    //create an empty n*m matrix. Value{i,j} =: None
    var mx = config.wrapper.emptyMatrix(n, m)

    //CAUTION: Do not merge both blocks

    //evaluate the matrix cells sequentially or concurrently
    config.dependencyCase match {
      case LEFT_UPLEFT_UP_UPRIGHT =>
        config.wrapper.computeMatrix(mx, initValues(0), n, m,
          config.dependencyCase, config.sleepPeriod, config.cellActorAmount, calcMatrixIndexValue)
      case UPLEFT_UP_UPRIGHT =>
        config.wrapper.computeMatrix(mx, initValues(0), m, n,
          config.dependencyCase, config.sleepPeriod, config.cellActorAmount, calcMatrixIndexValue)
      case _ =>
        for (k <- 0 until cellsSize) {
          val idx = getCellIndex(k)
          mx = calcMatrixIndexValue(mx, idx, parsePrevValues, handleNewValue)
        }
        mx
    }
  }


  /**
   * @see net.gumbix.dynpro.DynProConc.matlabMatrix
   */
  override lazy val matlabMatrix: Array[Array[Double]] =
    config.dependencyCase match {
    case NOT_CONCURRENT =>
      val mx: Array[Array[Double]] = Array.ofDim(n, m)
      for (i <- 0 until n; j <- 0 until m) {
        mx(i)(j) = matrix(i)(j) match {
          case a: Some[Double] => a.get
          case _ => 0
        }
      }
      mx

    case _ => config.wrapper.convertMatrix(matrix, n, m)
  }


  //Runtime.getRuntime().availableProcessors();
  /**
  Side note:
  It be extended to a concurrent method
  concept: extended version of the split & merge model
  TODO: define the concept (so far it is blurry in my head)
  Date: After successfully running the first alignment test.
   */

  /*
  override def solution(idx: Idx): List[PathEntry[Decision]] = config.dependencyCase match {
    case NOT_CONCURRENT => super.solution(idx)

    case _ => config.wrapper.calculateSolution(idx, calculateSolution)
  } */
}