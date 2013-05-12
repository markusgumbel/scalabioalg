package net.gumbix.dynpro.concurrency.threads

import scala.actors.Actor
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency._
import DependencyCase._

/**
 * An algorithm for parallel dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 11:34 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[concurrency] final class MatrixThread(mx: Array[Array[Option[Double]]], initValue: Double,
                         subMatrixAmount: Int, cellActorMatrixLength: Int,
                         dep: DependencyCase, sleepPeriod: Int, cellActorAmount: Int,
                         calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                          (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                          => Array[Array[Option[Double]]]
                         ) extends Actor with IMatrixComputer{


  override def act{

  }


  /**
   * @see concurrency.IMatrixComputer
   */
  override def computeMatrix: Array[Array[Option[Double]]] = Array()

}

