package net.gumbix.bioinf.string.seq.test

import net.gumbix.util.Logger
import net.gumbix.bioinf.string.seq.DoubleDigest
import org.junit.{Ignore, Test}

/**
  * Two examples for the double digest method.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class DoubleDigestDemo extends Logger {
  logLevel = true

  @Test
  @Ignore
  def testDoubleDigestDemo01() {
    val d = new DoubleDigest(List(1, 2, 3), List(2, 4), List(1, 1, 2, 2))
    d.printSolutions
  }

  @Test
  @Ignore
  def testDoubleDigestGumbel02() {
    val d = new DoubleDigest(List(1, 4, 3, 2),
      List(2, 2, 3, 3),
      List(1, 1, 2, 1, 2, 1, 2))
    d.printSolutions
  }
}