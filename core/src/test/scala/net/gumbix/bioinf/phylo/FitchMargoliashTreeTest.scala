package net.gumbix.bioinf.phylo

import org.junit.Test

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashTreeTest {
  val A = Array

  @Test
  def test2Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3),
      A(0d, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B"), d)
    buildTree(metric)
  }

  @Test
  def test3Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 4),
      A(0d, 0, 5),
      A(0d, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C"), d)
    buildTree(metric)
  }

  def test4Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 22, 24),
      A(0d, 0, 23, 25),
      A(0d, 0, 0, 4),
      A(0d, 0, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C", "D"), d)
    buildTree(metric)
  }

  @Test
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
    buildTree(metric)
  }

  /**
    * Additive tree.
    */
  @Test
  def test6Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 13, 12, 8, 9),
      A(0d, 0, 14, 13, 9, 10),
      A(0d, 0, 0, 9, 13, 14),
      A(0d, 0, 0, 0, 12, 13),
      A(0d, 0, 0, 0, 0, 5),
      A(0d, 0, 0, 0, 0, 0)
    )
    val metric =
      new FitchMargoliashMetric(Array("A", "B", "C", "D", "E", "F"), d)
    buildTree(metric)
  }

  /**
    * Not additive.
    */
  @Test
  def test6Taxa02() {
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
    buildTree(metric)
  }

  def buildTree(metric: FitchMargoliashMetric) {
    println("Additive? " + metric.isAdditive)
    val m = new FitchMargoliashTree(metric)
    m.logLevel = false // true
    val tree = m.allEdges
    println("Tree construction:")
    println(tree.map(e => e._1 + " = " + e._2).mkString("\n"))
  }
}