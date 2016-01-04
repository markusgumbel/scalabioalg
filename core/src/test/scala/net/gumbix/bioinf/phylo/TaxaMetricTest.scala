package net.gumbix.bioinf.phylo

import junit.framework.TestCase
import junit.framework.Assert._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class TaxaMetricTest extends TestCase {
  val A = Array
  val E = Double.NaN

  def testSimple() {
    val m = new TaxaMetric(Array("A"), Array.fill[Double](1, 1) {
      0
    })
    assertEquals(m.distByIndex(0, 0), E);
    assertEquals(m.distByTaxon("A", "A"), E);
  }

  def test2Taxa() {
    val d: Array[Array[Double]] = A(
      A(E, 1),
      A(E, E)
    )
    val m = new TaxaMetric(Array("A", "B"), d)
    assertEquals(E, m.distByIndex(0, 0));
    assertEquals(E, m.distByTaxon("A", "A"));
    assertEquals(1.0, m.distByTaxon("A", "B"));
  }

  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(E, 1, 2),
      A(E, E, 3),
      A(E, E, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(E, m.distByIndex(0, 0));
    assertEquals(1.0, m.distByIndex(0, 1));
    assertEquals(1.0, m.distByIndex(1, 0));
    assertEquals(E, m.distByTaxon("A", "A"));
    assertEquals(1.0, m.distByTaxon("A", "B"));
    assertEquals(2.0, m.distByTaxon("A", "C"));
    assertEquals(2.0, m.distByTaxon("C", "A"));
    println(m.mkMatrixString)
  }

  def test3TaxaB() {
    val d: Array[Array[Double]] = A(
      A(E, 1, 2),
      A(E, E, 3)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(m.distByIndex(0, 0), E);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), E);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
  }

  def testBunemanTrue01() {
    val d: Array[Array[Double]] = A(
      A(E, 3, 7, 8),
      A(E, E, 6, 7),
      A(E, E, E, 3),
      A(E, E, E, E)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E);
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
    assertEquals(m.distByIndex(0, 0), E);
    assertEquals(m.isAdditive, false);
  }
}