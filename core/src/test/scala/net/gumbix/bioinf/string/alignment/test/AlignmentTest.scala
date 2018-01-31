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

import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode}
import org.junit.{Ignore, Test}
import org.junit.Assert._

/**
  * Unit test cases for aligments.
  * Note: examples can be found in the demo module at
  * net.gumbix.bioinf.string.alignment.demo.*.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class AlignmentTest {

  @Test(expected = classOf[IllegalArgumentException])
  def testEmpty() {
    val dp = new Alignment("", "", AlignmentMode.GLOBAL)
    val res = dp.makeAlignmentString(dp.solution)
  }

  @Test
  def testOne() {
    val dp = new Alignment("A", "A", AlignmentMode.GLOBAL)
    val res = dp.makeAlignmentString(dp.solution)
    assertEquals("NB", dp.solution.map(_.decision).mkString(""))
    assertEquals(res, "A\n|\nA")
  }

  @Test
  def testOneTwo() {
    val dp = new Alignment("A", "AT", AlignmentMode.GLOBAL)
    val res = dp.makeAlignmentString(dp.solution)
    assertEquals("NBI", dp.solution.map(_.decision).mkString(""))
    assertEquals(res, "A~\n| \nAT")
  }

  @Test
  def testTwoOne() {
    val dp = new Alignment("AT", "A", AlignmentMode.GLOBAL)
    val res = dp.makeAlignmentString(dp.solution)
    assertEquals("NBD", dp.solution.map(_.decision).mkString(""))
    assertEquals(res, "AT\n| \nA~")
  }

  @Test
  def testTwoTwo1() {
    val dp = new Alignment("AT", "TA", AlignmentMode.GLOBAL)
    val res = dp.makeAlignmentString(dp.solution)
    assertEquals("NBB", dp.solution.map(_.decision).mkString(""))
    assertEquals(res, "AT\n  \nTA")
  }

  @Test
  @Ignore
  def localAlignment1() { // Detected exam ws 2017
    val s1 = "GCTATAC"
    val s2 = "TGCTCATA"
    val dp = new Alignment(s1, s2, AlignmentMode.LOCAL_OPTIMAL)
    val res = dp.makeAlignmentString(dp.solution)
    assertEquals("BBBIBBB", dp.solution.map(_.decision).mkString(""))
    assertEquals(res, ".GCT-ATAC\n ||| ||| \nTGCTCATA.")
  }
}