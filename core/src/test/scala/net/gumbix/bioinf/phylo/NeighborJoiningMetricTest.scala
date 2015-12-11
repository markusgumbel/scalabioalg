package net.gumbix.bioinf.phylo

import junit.framework.TestCase
import junit.framework.Assert._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class NeighborJoiningMetricTest extends TestCase {

  val A = Array

  def test3Taxa() {
    val d: Array[Array[Double]] = A(
      A(0, 1, 2, 2),
      A(0, 0, 1, 1),
      A(0, 0, 0, 3),
      A(0, 0, 0, 0)
    )
    val m = new NeighborJoiningMetric(Array("A", "B", "C", "D"), d)
    assertEquals(m.distByIndex(0, 0), 0.0);
    assertEquals(m.distByIndex(0, 1), 1.0);
    assertEquals(m.distByIndex(1, 0), 1.0);
    assertEquals(m.distByTaxon("A", "A"), 0.0);
    assertEquals(m.distByTaxon("A", "B"), 1.0);
    assertEquals(m.distByTaxon("A", "C"), 2.0);
    assertEquals(m.distByTaxon("C", "A"), 2.0);
    println(m.mkString)
    println(m.mkNJString)
    println(m.minIdx)
  }

}