package net.gumbix.bioinf.phylo

import junit.framework.TestCase
import junit.framework.Assert._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class NeighborJoiningMetricTest extends TestCase {

  val A = Array
  val E = Double.NaN

  def test4Taxa() {
    val d: Array[Array[Double]] = A(
      A(0d, 3, 22, 24),
      A(0d, 0, 23, 25),
      A(0d, 0, 0, 4),
      A(0d, 0, 0, 0)
    )
    val m = new NeighborJoiningMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E);
    assertEquals(m.distByIndex(0, 1), 3.0);
    assertEquals(m.distByIndex(1, 0), 3.0);
    assertEquals(m.distByTaxon("A", "A"), E);
    assertEquals(m.distByTaxon("A", "B"), 3.0);
    assertEquals(m.distByTaxon("A", "C"), 22.0);
    assertEquals(m.distByTaxon("C", "A"), 22.0);
    assertEquals(true, m.isAdditive);
    println(m.mkMatrixString)
    println("\nNJ matrix:\n" + m.mkNJString)
    println("\nMinimum distance has " + m.minTaxaGroup())
    println("\nAggregated matrix:\n" + m.clusterMinDistanceTaxa().mkMatrixString)
  }
}