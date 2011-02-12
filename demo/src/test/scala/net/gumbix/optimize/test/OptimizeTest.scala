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
package net.gumbix.optimize.test

import junit.framework.TestCase
import net.gumbix.optimize.{KnapsackRecMatrix, KnapsackRecursive}

/**
 * Here are only non DP solution. DP examples are
 * in net.gumbix.dynpro.test.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class OptimizeTest extends TestCase {
  /**
   * Like in PPT (ID = NEW2).
   */
  def testPPTExample() {
    val ks = new KnapsackRecursive(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
  }

  /**
   * Like in PPT (ID = NEW2).
   */
  def testPPTExampleMatrix() {
    val ks = new KnapsackRecMatrix(Array("A", "B", "C", "D"),
      Array(2, 2, 6, 5), Array(6, 3, 5, 4), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
    println()
    println(ks.mkMatrixString)
  }

  /**
   * Like in older version of PPT.
   */
  def testOldPPTExample() {
    val ks = new KnapsackRecursive(Array("A", "B", "C", "D"),
      Array(5, 6, 2, 2), Array(4, 5, 3, 6), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
  }

  /**
   * Like in older version of PPT.
   */
  def testOldPPTExampleMatrix() {
    val ks = new KnapsackRecMatrix(Array("A", "B", "C", "D"),
      Array(5, 6, 2, 2), Array(4, 5, 3, 6), 10)
    val (items, value) = ks.calculateSolution
    println(items)
    println("value = " + value)
    println(ks.mkOverviewString)
    println()
    println(ks.mkMatrixString)
  }
}