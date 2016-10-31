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

import junit.framework.TestCase

/**
 * Several examples for dynamic programming. Mainly the knapsack problem
 * and the calculation of Fibonacci numbers. 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class DynProDemo extends TestCase {

  /**
   * Values are taken from Saake & Sattler, p. 222.
   */
  def testKnapsackDPSaakeSattler222() {
    val dp = new KnapsackDynPro(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    doKnapsack(dp)
  }

  /**
   * Reverse order.
   * Values are taken from Saake & Sattler, p. 222. 
   */
  def testKnapsackDPSaakeSattler222Reverse() {
    val dp = new KnapsackDynPro(Array("A", "B", "C", "D"),
      Array(5, 6, 2, 2), Array(4, 5, 3, 6), 10)
    doKnapsack(dp)
  }

  /**
   * Values are taken from Saake & Sattler, p. 222.
   */
  def testKnapsackDPSaakeSattler222Backwards() {
    val dp = new KnapsackDynProBackwards(Array("A", "B", "C", "D"),
      Array(5, 6, 2, 2), Array(4, 5, 3, 6), 10)
    doKnapsack(dp)
  }

  /**
   * As in Saake & Sattler, p. 224f.
   */
  def testKnapsackDPSaakeSattler224() {
    val dp = new KnapsackDynProBackwards(Array("1", "2", "3", "4", "5"),
      Array(2, 2, 6, 5, 4), Array(6, 3, 5, 4, 6), 10)
    doKnapsack(dp)
  }

  /**
   * As in Morlock & Neumann, p. 610f.
   */
  def testKnapsackNeumannMorlock610() {
    val dp = new KnapsackDynProBackwards(Array("1", "2", "3", "4"),
      Array(2, 2, 4, 6), Array(8, 6, 10, 12), 11)
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

  /**
   * Calculate a Fibonacci number. 
   */
  def testFibonacci() {
    val dp = new FibonacciDynPro(8)
    println(dp.mkMatrixString(dp.solution))
    println()
  }
}