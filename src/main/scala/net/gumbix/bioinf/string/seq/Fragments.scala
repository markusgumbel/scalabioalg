package net.gumbix.bioinf.string.seq

import collection.mutable.ArrayBuffer
import util.{Random, Sorting}

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 * @param s
 * @param count How many repeats?
 * @param frgmtSize The average size of a fragment.
 * @param seed The seed value of the random number generator.
 */
class Fragments(s: String, count: Int, frgmtSize: Int, seed: Long) {
  def this(s: String, count: Int, frgmtSize: Int) =
    this (s, count, frgmtSize, System.currentTimeMillis)

  val sepFragments = {
    val random = new java.util.Random(seed)
    for (k <- 1 to count) yield {
      val cuts = s.size / frgmtSize
      val cutPositions = for (i <- 1 to cuts) yield {
        random.nextInt.abs % s.size
      }
      val sortedPositions = cutPositions.sorted.toArray
      val positions = Array.concat(Array(0), sortedPositions, Array(s.size))
      val fragments = for (i <- 0 until positions.size - 1) yield {
        s.substring(positions(i), positions(i + 1))
      }
      // println(fragments.mkString(", "))
      fragments
    }
  }

  val fragments = sepFragments.flatten

  val randomFragments = {
    Sorting.stableSort(fragments,
      (a: String, b: String) => Random.nextInt(2) == 1)
  }
}