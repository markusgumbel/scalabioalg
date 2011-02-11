package net.gumbix.bioinf.string.alignment.test

import junit.framework.TestCase
import net.gumbix.bioinf.string.dotplot.DotPlot2
import net.gumbix.bioinf.string.alignment.AlignmentMode._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class AlignmentDemoTest extends TestCase with ExampleData with AlignmentOutput {

  /**
   * Use this test case to run an dot plot.
   */
  def xtestDotPlot() {
    val (s1, s2, comment) = strings("Huett-p151")
    new DotPlot2(s1, s2, comment)
    Thread.sleep(5 * 60 * 1000)
  }

  /**
   * Use this test case to run an alignment.
   */
  def testAlignment() {
    val (s1, s2, comment) = strings("Huett-p151")
    doAligmentDP(GLOBAL, s1, s2, comment)
  }

  /**
   * Semiglobal alignment.
   */
  def testSeminglobal01() {
    val (s1, s2, comment) = strings("Gumbel-semi-1")
    doAligmentDP(GLOBAL, s1, s2, comment)
    doAligmentDP(LOCAL_CENTERED, s1, s2, comment)
    doAligmentDP(LOCAL_OPTIMAL, s1, s2, comment)
  }

  def testKW10Alignment() {
    val s1 = "ARAYC"
    val s2 = "AARAC"
    doAligmentDP(GLOBAL, s1, s2, "Klausur 10")
  }
}