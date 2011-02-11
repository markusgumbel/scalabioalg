package net.gumbix.bioinf.string.alignment.test

import junit.framework.TestCase
import net.gumbix.bioinf.string.alignment.AlignedString
import net.gumbix.bioinf.string.alignment.GapType._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class AlignedStringTest extends TestCase {
  def testAlignedString01() {

    val als = new AlignedString("hallo")

    //assertTrue(als.size == 5)

    als.insertGapBefore(0, 5, GAP)

    //assertTrue(als.size == 10)

    //assertTrue(als.toString() == "-----hallo")
    println("as = " + als)

    als.insertGapBefore(6, 1, GAP)
    println("as = " + als)

    als.insertGapBefore(als.size, 4, GAP)
    println("as = " + als)

    //assertTrue(als(5) == "h")

    //assertTrue(als.primaryString == "hallo")
  }
}