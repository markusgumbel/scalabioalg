/*
Copyright 2011 the original author or authors.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package net.gumbix.bioinf.string.alignment.test

import net.gumbix.bioinf.string.alignment.{AlignmentPrinter, AlignedString}
import net.gumbix.bioinf.string.alignment.GapType._
import org.junit.Test
import org.junit.Assert._

/**
 * Test cases for aligned strings.
 * TODO Write more!
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class AlignedStringTest {

  @Test
  def testAlignedString() {

    val als = new AlignedString("hello")
    assertTrue(als.size == 5)

    als.insertGapBefore(0, 5, GAP)
    assertTrue(als.size == 10)
    assertEquals(als.toString(), "-----hello")

    als.insertGapBefore(6, 1, GAP)
    assertEquals(als.toString, "-----h-ello")

    als.insertGapBefore(als.size, 4, GAP)
    assertEquals(als.toString(), "-----h-ello----")
    assertEquals(als(5).toString, "h")
    assertEquals(als.primaryString, "hello")
  }

  @Test
  def testParserNoGaps() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals("ACTGAG", s1.toString)
    assertEquals(0, s1.gaps().size)
  }

  @Test
  def testParser1Gap() {
    val s1 = new AlignedString("AC-TGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(2), true)
    assertEquals(1, s1.gaps().size)
    assertEquals(2, s1.gaps()(0))
  }

  @Test
  def testParser2Gap() {
    val s1 = new AlignedString("AC--TGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(2), true)
    assertEquals(s1.isGapAt(3), true)
  }

  @Test
  def testParserGapBeginning() {
    val s1 = new AlignedString("-ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.size, 7)
    assertEquals(s1.isGapAt(0), true)
    assertEquals(s1.isGapAt(6), false)
  }

  @Test
  def testParserGapEnd() {
    val s1 = new AlignedString("ACTGAG-")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.size, 7)
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(6), true)
  }

  @Test
  def testInsert1Gap() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    s1.insertGapBefore(1, 1, GAP)
    assertEquals(s1.toString(), "A-CTGAG")
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(1), true)
    assertEquals(s1.isGapAt(2), false)
  }

  @Test
  def testInsert2Gap() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    s1.insertGapBefore(1, 1, GAP)
    assertEquals(s1.toString(), "A-CTGAG")
    s1.insertGapBefore(2, 1, GAP)
    assertEquals(s1.toString(), "A--CTGAG")
  }

  @Test
  def testInsert2bGap() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    s1.insertGapBefore(1, 2, GAP)
    assertEquals(s1.toString(), "A--CTGAG")
  }

  @Test
  def testInsertxGap() {
    val s1 = new AlignedString("ACAGGCT")
    assertEquals(s1.primaryString, "ACAGGCT")
    s1.insertGapBefore(2, 1, GAP)
    assertEquals(s1.toString(), "AC-AGGCT")
    s1.insertGapBefore(3, 1, GAP)
    assertEquals(s1.toString(), "AC--AGGCT")
  }

  @Test
  def testInsert2GapsWithGap() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    s1.insertGapBefore(1, 1, GAP)
    s1.insertGapBefore(3, 1, GAP)
    assertEquals(s1.toString(), "A-C-TGAG")
  }
}