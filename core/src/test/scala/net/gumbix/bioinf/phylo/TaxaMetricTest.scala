package net.gumbix.bioinf.phylo

import org.junit.Test
import org.junit.Assert._

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class TaxaMetricTest {
  val DELTA = 0.00001
  val A = Array
  val E = Double.NaN

  @Test
  def testSimple() {
    val m = new TaxaMetric(Array("A"), Array.fill[Double](1, 1) {
      0
    })
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.distByTaxon("A", "A"), E, DELTA)
  }

  @Test
  def test2Taxa() {
    val d: Array[Array[Double]] = A(
      A(E, 1),
      A(E, E)
    )
    val m = new TaxaMetric(Array("A", "B"), d)
    assertEquals(E, m.distByIndex(0, 0), DELTA)
    assertEquals(E, m.distByTaxon("A", "A"), DELTA)
    assertEquals(1.0, m.distByTaxon("A", "B"), DELTA)
  }

  @Test
  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(E, 1, 2),
      A(E, E, 3),
      A(E, E, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(E, m.distByIndex(0, 0), DELTA)
    assertEquals(1.0, m.distByIndex(0, 1), DELTA)
      assertEquals(1.0, m.distByIndex(1, 0), DELTA)
      assertEquals(E, m.distByTaxon("A", "A"), DELTA)
      assertEquals(1.0, m.distByTaxon("A", "B"), DELTA)
      assertEquals(2.0, m.distByTaxon("A", "C"), DELTA)
      assertEquals(2.0, m.distByTaxon("C", "A"), DELTA)
  }

  @Test
  def test3TaxaB() {
    val d: Array[Array[Double]] = A(
      A(E, 1, 2),
      A(E, E, 3)
    )
    val m = new TaxaMetric(Array("A", "B", "C"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.distByIndex(0, 1), 1.0, DELTA)
    assertEquals(m.distByIndex(1, 0), 1.0, DELTA)
    assertEquals(m.distByTaxon("A", "A"), E, DELTA)
    assertEquals(m.distByTaxon("A", "B"), 1.0, DELTA)
    assertEquals(m.distByTaxon("A", "C"), 2.0, DELTA)
    assertEquals(m.distByTaxon("C", "A"), 2.0, DELTA)
  }

  @Test
  def testBunemanTrue01() {
    val d: Array[Array[Double]] = A(
      A(E, 3, 7, 8),
      A(E, E, 6, 7),
      A(E, E, E, 3),
      A(E, E, E, E)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.isAdditive, true)
  }

  @Test
  def testBunemanFalse01() {
    val d: Array[Array[Double]] = A(
      A(0.0, 3, 9, 7),
      A(0.0, 0, 9, 8),
      A(0.0, 0, 0, 6),
      A(0.0, 0, 0, 0)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.isAdditive, false)
  }

  @Test
  def testUltraTrue01() {
    val d: Array[Array[Double]] = A(
      A(E, 6, 6, 8),
      A(E, E, 2, 8),
      A(E, E, E, 8),
      A(E, E, E, E)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.isAdditive, true)
    assertEquals(m.isUltrametric, true)
  }

  @Test
  def testUltraFalse01() {
    val d: Array[Array[Double]] = A(
      A(E, 3, 7, 8),
      A(E, E, 7, 8),
      A(E, E, E, 3),
      A(E, E, E, E)
    )
    val m = new TaxaMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA)
    assertEquals(m.isAdditive, true)
    assertEquals(m.isUltrametric, false)
  }
}