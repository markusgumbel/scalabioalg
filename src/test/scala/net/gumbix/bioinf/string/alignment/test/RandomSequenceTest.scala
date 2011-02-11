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
import net.gumbix.bioinf.hmm.HiddenMarkovAutomata

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class RandomSequenceTest extends TestCase {
  // q0 and n(ormal)
  val transCoin = Array(Array(0.0, 1.0), Array(0.0, 1.0))

  val emmCoin = Array(Array(0.5, 0.5))

  val alphabetCoin = "10".toArray
  val statesCoin = Array('n')

  def testGenerateOutput() {
    val hmm = new HiddenMarkovAutomata(alphabetCoin, statesCoin,
      transCoin, emmCoin)
    val (states, chars) = hmm.makeString(100)
    print(chars.mkString(" "))
    println
  }
}