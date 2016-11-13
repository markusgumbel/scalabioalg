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
package net.gumbix.optimize

import org.junit.{Ignore, Test}

/**
  * Here are some examples for optimization problems, mainly those
  * who could be (better) solved with dynamic programming.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class OptimizeDemo {
  /**
    * The knapsack problem using a specific recursive algorithm.
    */
  @Test
  @Ignore
  def testKnapsack() {
    val ks = new KnapsackRecursive(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
  }

  /**
    * The knapsack problem solved by a recursive algorithm
    * that uses a matrix to store results (like in dynamic programming).
    */
  @Test
  @Ignore
  def testKnapsackMatrix() {
    val ks = new KnapsackRecMatrix(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
    println()
    println(ks.mkMatrixString)
  }
}