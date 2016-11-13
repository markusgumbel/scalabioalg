package net.gumbix.bioinf.string.seq.test

import net.gumbix.util.Logger
import net.gumbix.bioinf.string.seq.DoubleDigest
import org.junit.{Ignore, Test}

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */

class SeqDemo extends Logger {
  logLevel = true

  @Test
  @Ignore
  def testDoubleDigestDemo() {
    val d = new DoubleDigest(List(1, 2, 3), List(2, 4), List(1, 1, 2, 2))
    d.printSolutions
  }

  /**
    * My own example. Produces 20 solutions.
    */
  @Test
  @Ignore
  def testDoubleDigestGumbel01() {
    val d = new DoubleDigest(List(1, 4, 3, 2),
      List(2, 2, 3, 3),
      List(1, 1, 2, 1, 2, 1, 2))
    d.printSolutions
  }

  /**
    * My own example as used in the exercise variant 3. Produces 8 out of 24 solutions.
    */
  @Test
  @Ignore
  def testDoubleDigestGumbel03() {
    val d = new DoubleDigest(List(2, 2, 3, 4),
      List(2, 2, 3, 3),
      List(1, 1, 2, 1, 2, 2, 2))
    d.printSolutions
  }
}