package net.gumbix.dynpro.concurrency.threads

import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import ConClass._

/**
 * An algorithm for parallel dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:34 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[concurrency] final class MatrixThread(mx: Array[Array[Option[Double]]], initVal: Double,
                         val clazz: ConClass,
                         calcCellCost:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                         ){



  /**
   * @see concurrency.IMatrixComputer
   */
  def computeMatrix: Array[Array[Option[Double]]] = Array()

}

