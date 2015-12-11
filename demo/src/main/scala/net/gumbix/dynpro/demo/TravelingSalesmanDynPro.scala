package net.gumbix.dynpro.demo

import net.gumbix.dynpro.{PathEntry, Idx, MatrixPrinter, DynPro}
import scala.Array

/**
 * Under construction!
 * (c) 2012 by Markus Gumbel (m.gumbel@hs-mannheim.de)
 * @author Markus Gumbel
 */

class TravelingSalesmanDynPro(val dist: Array[Array[Int]])
  extends DynPro[Int] with MatrixPrinter[Int] {

  formatter = DECIMAL

  def n = dist.length

  def m = 100 // TODO

  /**
   * Minimize the trip length.
   */
  override def extremeFunction = MIN

  def decisions(idx: Idx) = {
    Array(0)
  }

  def prevStates(idx: Idx, d: Int) = {
    Array()
  }

  def value(idx: Idx, d: Int) = dist(idx.i)(idx.j)

  def solution: List[PathEntry[Int]] = solution(Idx(n - 1, 0))
}
