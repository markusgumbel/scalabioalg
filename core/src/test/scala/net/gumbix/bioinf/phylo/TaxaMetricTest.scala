package net.gumbix.bioinf.phylo

import junit.framework.TestCase
import junit.framework.Assert._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class TaxaMetricTest extends TestCase {
  val A = Array

  def testSimple() {
    val m = new TaxaMetric(Array("A"), Array.fill[Double](1, 1) {
      0
    })
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
  }

  def test2Taxa() {
    val d: Array[Array[Double]] = A(
      A(0, 1),
      A(0, 0)
    )
    val m = new TaxaMetric(Array("A", "B"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
  }

  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(0, 1, 2),
      A(0, 0, 3),
      A(0, 0, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
    println(m.mkString)
  }

  def test3TaxaB() {
    val d: Array[Array[Double]] = A(
      A(0, 1, 2),
      A(0, 0, 3)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
  }
}