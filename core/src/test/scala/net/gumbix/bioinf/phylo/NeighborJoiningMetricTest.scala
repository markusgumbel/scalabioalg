package net.gumbix.bioinf.phylo

import org.junit.Test
import org.junit.Assert._

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class NeighborJoiningMetricTest {

  val DELTA = 0.00001
  val A = Array
  val E = Double.NaN

  @Test
  def test4Taxa() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 22, 24),
      A(0d, 0, 23, 25),
      A(0d, 0, 0, 4),
      A(0d, 0, 0, 0)
    )
    val m = new NeighborJoiningMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E, DELTA);
    assertEquals(m.distByIndex(0, 1), 3.0, DELTA);
    assertEquals(m.distByIndex(1, 0), 3.0, DELTA);
    assertEquals(m.distByTaxon("A", "A"), E, DELTA);
    assertEquals(m.distByTaxon("A", "B"), 3.0, DELTA);
    assertEquals(m.distByTaxon("A", "C"), 22.0, DELTA);
    assertEquals(m.distByTaxon("C", "A"), 22.0, DELTA);
    assertEquals(true, m.isAdditive);
    println(m.mkMatrixString)
    println("\nNJ matrix:\n" + m.mkNJString)
    println("\nMinimum distance has " + m.minTaxaGroup())
    println("\nAggregated matrix:\n" + m.clusterMinDistanceTaxa().mkMatrixString)
  }
}