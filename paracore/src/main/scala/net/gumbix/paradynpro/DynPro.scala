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

abstract class DynPro[Decision](paraType: ParaType, dependencyCase: DependencyCase) extends net.gumbix.dynpro.DynPro {

  val cellActorAmount = 0


  /**
   *
   */
  override lazy val matrix: Array[Array[Option[Double]]] = {
    val mx: Array[Array[Option[Double]]] = Array.ofDim(n, m)
    for (i <- 0 until n; j <- 0 until m) {
      mx(i)(j) = None
    }

    dependencyCase match {
      case LEFT_UPLEFT_UP | UPLEFT_UP_UPRIGHT =>
        ParaWrapper.computeMatrix(paraType, mx, initValues(0), dependencyCase, cellActorAmount, calcMatrixIndexValue)
      case LEFT_UPLEFT_UP_UPRIGHT | none => super.matrix

    }
  }



  //TODO override the following methods: matrix and possibly matlabmatrix, solution

}