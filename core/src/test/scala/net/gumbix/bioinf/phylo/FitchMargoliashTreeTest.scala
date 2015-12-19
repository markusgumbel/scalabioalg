package net.gumbix.bioinf.phylo

import junit.framework.TestCase

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashTreeTest extends TestCase {
  val A = Array

  def test4Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 22, 24),
      A(0d, 0, 23, 25),
      A(0d, 0, 0, 4),
      A(0d, 0, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C", "D"), d)
    println("Additive? " + metric.isAdditive)
    val m = new FitchMargoliashTree(metric)
    m.tree()
  }

  def test5Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 22, 39, 39, 41),
      A(0d, 0, 41, 41, 43),
      A(0d, 0, 0, 18, 20),
      A(0d, 0, 0, 0, 10),
      A(0d, 0, 0, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C", "D", "E"), d)
    println("Additive? " + metric.isAdditive)
    val m = new FitchMargoliashTree(metric)
    m.tree()
  }

  /**
    * Not additive.
    */
  def test6Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 22, 39, 39, 41, 20),
      A(0d, 0, 41, 41, 43, 21),
      A(0d, 0, 0, 18, 20, 19),
      A(0d, 0, 0, 0, 10, 5),
      A(0d, 0, 0, 0, 0, 4),
      A(0d, 0, 0, 0, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C", "D", "E", "F"), d)
    println("Additive? " + metric.isAdditive)
    val m = new FitchMargoliashTree(metric)
    m.tree()
  }
}