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
package net.gumbix.dynpro.test

import junit.framework.Assert._
import junit.framework.TestCase
import net.gumbix.bioinf.string.alignment.Alignment
import net.gumbix.bioinf.string.alignment.AlignmentMode._

/**
 * Test cases to verify the dynamic programming algorithm.
 * Note: examples can be found in the demo module at
 * net.gumbix.dynpro.demo.*.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class DynProTest extends TestCase {
  def testDummy() {
    // TODO
  }

  def testLaTeXMatrix() {
    val s1 = "CTGAGACATTACTG"
    val s2 = "AGCAGACTTACCA"
    val dp = new Alignment(s1, s2, GLOBAL)
    val latex = dp.mkMatrixLaTeXStringSolution(dp.solution)
    println(latex)
    val (as1, as2) = dp.alignedStrings()
    println(dp.makeLaTeXAlignmentString(as1, as2))
    assertEquals(true, true)
  }
}