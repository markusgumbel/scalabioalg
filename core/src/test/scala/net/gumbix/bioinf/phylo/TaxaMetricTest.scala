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
      A(0d, 1),
      A(0d, 0)
    )
    val m = new TaxaMetric(Array("A", "B"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
  }

  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(0d, 1, 2),
      A(0d, 0, 3),
      A(0d, 0, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
    println(m.mkMatrixString)
  }

  def test3TaxaB() {
    val d: Array[Array[Double]] = A(
      A(0.0, 1, 2),
      A(0.0, 0, 3)
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

  def testBunemanTrue01() {
    val d: Array[Array[Double]] = A(
      A(0.0, 3, 7, 8),
      A(0.0, 0, 6, 7),
      A(0.0, 0, 0, 3),
      A(0.0, 0, 0, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.isAdditive, true);
  }

  def testBunemanFalse01() {
    val d: Array[Array[Double]] = A(
      A(0.0, 3, 9, 7),
      A(0.0, 0, 9, 8),
      A(0.0, 0, 0, 6),
      A(0.0, 0, 0, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.isAdditive, false);
  }
}