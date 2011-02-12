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
import net.gumbix.bioinf.string.alignment.AlignmentMode._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class AlignmentDemoTest extends TestCase with ExampleData with AlignmentOutput {

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