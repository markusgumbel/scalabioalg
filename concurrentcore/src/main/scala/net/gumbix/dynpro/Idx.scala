package net.gumbix.dynpro

import math.{min, max}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 8/1/13
 * Time: 3:06 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 *
 * Convenient class for a two-dimensional index pair (i, j).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
case class Idx(i: Int, j: Int) {
  def +(o: Idx) = Idx(i + o.i, j + o.j)
  def -(o: Idx) = Idx(i - o.i, j - o.j)

  def +(z: Int) = Idx(i + z, j + z)
  def -(z: Int) = Idx(i - z, j - z)

  def +(zi: Int, zj: Int) = Idx(i + zi, j + zj)
  def -(zi: Int, zj: Int) = Idx(i - zi, j - zj)

  def ==(o: Idx) = (i == o.i && j == o.j)

  val MIN = min(i, j)
  val MAX = max(i, j)

  override def toString = "(" + i + ", " + j + ")"
}
