package net.gumbix.bioinf.phylo

import org.junit.Test
import org.junit.Assert.assertEquals

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class NeighborJoiningTreeTest {
  val A = Array

  @Test
  def test2Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3),
      A(0d, 0)
    )
    val metric =
      new NeighborJoiningMetric(Array("A", "B"), d)
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
      new NeighborJoiningMetric(Array("A", "B", "C"), d)
    buildTree(metric)
  }

  @Test
  def test4TaxaDurbin() {
    val d: Array[Array[Double]] = A(
      A(0d, 0.3, 0.5, 0.6),
      A(0d, 0.0, 0.6, 0.5),
      A(0d, 0.0, 0.0, 0.9),
      A(0d, 0.0, 0.0, 0.0)
    )
    val metric =
      new NeighborJoiningMetric(Array("1", "2", "3", "4"), d)
    val m = new NeighborJoiningTree(metric)
    val edges = m.allEdges.toMap
    assertEquals(edges(new Taxon("1")), 0.1, 0.01)
    assertEquals(edges(new Taxon("2")), 0.1, 0.01)
    assertEquals(edges(new Taxon("3")), 0.4, 0.01)
    assertEquals(edges(new Taxon("4")), 0.4, 0.01)
    // TODO I do not understand why the edge {1,3}{2,4} is missing:
    val jt1 = new JoinedTaxon(Array(new Taxon("1"), new Taxon("3")))
    //val jt2 = new JoinedTaxon(Array(new Taxon("2"), new Taxon("4")))
    assertEquals(edges(jt1), 0.1, 0.01)
  }

  @Test
  def test4Taxa01() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 22, 24),
      A(0d, 0, 23, 25),
      A(0d, 0, 0, 4),
      A(0d, 0, 0, 0)
    )
    val metric =
      new NeighborJoiningMetric(Array("A", "B", "C", "D"), d)
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
      new NeighborJoiningMetric(Array("A", "B", "C", "D", "E"), d)
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
      new NeighborJoiningMetric(Array("A", "B", "C", "D", "E", "F"), d)
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
      new NeighborJoiningMetric(Array("A", "B", "C", "D", "E", "F"), d)
    buildTree(metric)
  }

  def buildTree(metric: NeighborJoiningMetric) {
    println("Additive? " + metric.isAdditive)
    val m = new NeighborJoiningTree(metric)
    m.logLevel = true // true
    val tree = m.allEdges
    println("Tree construction:")
    println(tree.map(e => e._1 + " = " + e._2).mkString("\n"))
  }
}