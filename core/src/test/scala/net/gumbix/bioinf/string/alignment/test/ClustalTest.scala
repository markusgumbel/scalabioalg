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

import net.gumbix.bioinf.string.alignment.{Clustal, StarAlignment}
import org.junit.Assert._
import org.junit.Test

/**
 * Unit test cases for multiple aligments.
 * Note: examples can be found in the demo module at
 * net.gumbix.bioinf.string.alignment.demo.*.
 *
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class ClustalTest extends MultipleAlignmentData {

  @Test
  def testPair() {
    val msa = new Clustal(seqs("pair"))
    assertEquals("CAT", msa.consensus)
    assertEquals((-2+1+1), msa.sumOfPairs)
    assertEquals(1, msa.distance)
  }
}