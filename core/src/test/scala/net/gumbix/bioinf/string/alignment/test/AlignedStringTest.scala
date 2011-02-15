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

import junit.framework.TestCase
import junit.framework.Assert._
import net.gumbix.bioinf.string.alignment.AlignedString
import net.gumbix.bioinf.string.alignment.GapType._

/**
 * Test cases for aligned strings.
 * TODO Write more!
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class AlignedStringTest extends TestCase {
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
}