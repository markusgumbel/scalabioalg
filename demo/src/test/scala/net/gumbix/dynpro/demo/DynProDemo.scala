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
package net.gumbix.dynpro.demo

import org.junit.{Ignore, Test}

/**
  * Some examples of the knapsack problem to demonstrate
  * the dynamic programming algorithm.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class DynProDemo {

  /**
    * Values are taken from Saake & Sattler, p. 222.
    */
  @Test
  @Ignore
  def testKnapsackDPSaakeSattler222() {
    val dp = new KnapsackDynPro(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    doKnapsack(dp)
  }


  /**
    * Some nice output.
    */
  def doKnapsack(dp: KnapsackDynPro) {
    val solution = dp.solution
    println(dp.mkOverviewString)
    println()
    println(dp.mkMatrixString(solution))
    println()
    solution.foreach(println)
  }
}