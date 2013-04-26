package net.gumbix.paradynpro

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/15/13
 * Time: 3:42 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

import actors.MatrixActor
import DependencyCase._
import ParaType._

abstract class DynPro[Decision] extends net.gumbix.dynpro.DynPro {

  //TO OVERRIDE IF NECESSARY
  val paraType = ACTOR
  val dependencyCase = LEFT_UPLEFT_UP_UPRIGHT
  // cellActorAmount < 1 => realCellActorAmount = amount of submatrices
  val cellActorAmount = 0
  /*
  period (in milliseconds) that a CellActor has to wait if all values it is dependant from
  haven't been computed yet.
  */
  val waitPeriod = 10


  /**
   * @see net.gumbix.dynpro.DynPro.matrix
   */
  override lazy val matrix: Array[Array[Option[Double]]] = {
    val mx: Array[Array[Option[Double]]] = Array.ofDim(n, m)
    for (i <- 0 until n; j <- 0 until m) {
      mx(i)(j) = None
    }

    dependencyCase match {
      case LEFT_UPLEFT_UP_UPRIGHT | UPLEFT_UP_UPRIGHT =>
        ParaWrapper.computeMatrix(paraType, mx, initValues(0), dependencyCase, waitPeriod, cellActorAmount, calcMatrixIndexValue)
      case _ => super.matrix

    }
  }



  //TODO override the following methods: matrix and possibly matlabmatrix, solution

}