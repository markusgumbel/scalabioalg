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

import net.gumbix.bioinf.string.alignment.StarAlignment
import org.junit.Test
import org.junit.Assert._

/**
  * Unit test cases for multiple aligments.
  * Note: examples can be found in the demo module at
  * net.gumbix.bioinf.string.alignment.demo.*.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class StarAlignmentTest extends MultipleAlignmentData {

  @Test
  def testPair() {
    val msa = new StarAlignment(seqs("pair"))
    assertEquals("CAT", msa.consensus)
    assertEquals((-2 + 1 + 1), msa.sumOfPairs)
    assertEquals(1, msa.distance)
  }

  @Test
  def testThree1() {
    // "CAT", "AT", "THAT"
    val msa = new StarAlignment(seqs("three1"), true)
    // CAT;CAT
    assertEquals(1 + 1 + 1, msa.alignments(0)(0).similarity, 0.01)
    // CAT;AT
    assertEquals(-2 + 1 + 1, msa.alignments(0)(1).similarity, 0.01)
    // CAT;THAT, this is ambiguous; we expect:
    // C-AT
    // THAT
    assertEquals(-1 - 2 + 1 + 1, msa.alignments(0)(2).similarity, 0.01)

    assertEquals("CHAT", msa.consensus)
    val sp = (-1 - 2 - 2) + (-2 + 1 - 2) + (1 + 1 + 1) + (1 + 1 + 1)
    assertEquals(sp, msa.sumOfPairs)
    assertEquals(2 + 2 + 0 + 0, msa.distance)
  }
}