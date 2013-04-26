package net.gumbix.paradynpro

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:07 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

import DependencyCase._
import ParaType._
import net.gumbix.dynpro.Idx
import net.gumbix.paradynpro.actors.MatrixActor
import net.gumbix.paradynpro.threads.MatrixThread


protected[paradynpro] object ParaWrapper {

  /**
   * This method creates a master object (MatrixActor or MatrixThread) depending on the given
   * paraType, which then computes the cell values of the given matrix.
   *
   * @param paraType ACTOR or THREAD.
   * @param mx The matrix used to store all the computed values.
   * @param initValue The value used @ the beginning of the matrix (default value).
   * @param dependencyCase One of the 3 possible dependency cases.
   * @param cellActorAmount The amount of cell actors allowed to concurrently compute on the matrix "mx".
   * @param calcMatrixIndexValue The method used to compute the value of each cells.
   * @return
   */
  def computeMatrix(paraType: ParaType,
                    mx: Array[Array[Option[Double]]],
                    initValue: Double,
                    dependencyCase: DependencyCase,
                    waitPeriod: Int,
                    cellActorAmount: Int,
                    calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                      (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                      => Array[Array[Option[Double]]]): Array[Array[Option[Double]]] ={

    val para: IMatrixComputer = paraType match {
      case ACTOR =>
        new MatrixActor(mx, initValue, dependencyCase, waitPeriod, cellActorAmount, calcMatrixIndexValue)
      case THREAD =>
        new MatrixThread(mx, initValue, dependencyCase, waitPeriod, cellActorAmount, calcMatrixIndexValue)
    }
    para.computeMatrix
  }

}

