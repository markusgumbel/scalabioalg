package net.gumbix.bioinf.phylo

import junit.framework.Assert._
import junit.framework.TestCase

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class FitchMargoliashMetricTest extends TestCase {

  val A = Array
  val E = Double.NaN

  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(E, 1, 2, 2),
      A(E, E, 1, 1),
      A(E, E, E, 3),
      A(E, E, E, E)
    )
    val m = new FitchMargoliashMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), E);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), E);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
    assertEquals(m.isAdditive, false); // Really?
    println(m.mkMatrixString)
    val im = m.minDistanceIdx(0)
    val jm = m.minDistanceIdx(1)
    println("\nminimum distance is dist(" + m.taxa(im) + ", " +
      m.taxa(jm) + ") = " + m.distByIndex(im, jm) + " at index " +
      m.minDistanceIdx.toString)
    println("\nMetric of size 3 to solve directly:")
    val nmMetric = m.clusterNonMinDistanceTaxa
    println("\n" + nmMetric.mkMatrixString)
    println("\nMetric of size n - 1:")
    val mMetric = m.clusterMinDistanceTaxa
    println("\n" + mMetric.mkMatrixString)
  }
}