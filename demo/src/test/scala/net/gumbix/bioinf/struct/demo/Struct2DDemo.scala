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
package net.gumbix.bioinf.struct.demo

import net.gumbix.dynpro.Idx
import net.gumbix.bioinf.struct.NussinovCount
import org.junit.{Ignore, Test}

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class Struct2DDemo {
  /**
   * human tRNA
   * http://www.ebi.ac.uk/ena/data/view/M31637&display=text
   */
  val tRNA = "UCCCUGGUGGUCUAGUGGDUAGGAUUCGGCGCUCUCACCGCCGCGGCCCGGGUUCGAUUCCCGGUCAGGGAACCA"

  @Test
  @Ignore
  def testHumantRNA() {
    doStructure(tRNA)
  }

  def doStructure(s: String) {
    println("\n---")
    val dp = new NussinovCount(s)
    val solution = dp.solution(Idx(0, dp.n - 1))
    solution.foreach(e => println(e.decision))
    println(dp.mkMatrixString(solution))
  }
}