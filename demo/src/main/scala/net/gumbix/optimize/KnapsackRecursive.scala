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

/**
 * The knapsack algorithm. Several branches are computed redundantly.  
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class KnapsackRecursive(val items: Array[String],
                        val weights: Array[Int], val values: Array[Int],
                        val capacity: Int)
        extends Knapsack {
  def knapsackSolution = {
    val (items, value) = calculateSolution
    items.map(x => if (x._2) 1 else 0).toArray
  }

  /**
   * @return A pair consisting of 1) a list of tuples with the item to take
   * and the decision and 2) the value of this solution.
   */
  def calculateSolution: Tuple2[List[Tuple2[String, Boolean]], Int] = {

    /**
     * @param i Which item to pick? i = 0 .. n-1
     * @param remainingCapacity The remaining capacity of the knapsack when
     * i-1 items were already considered.
     */
    def knapsack(i: Int, remainingCapacity: Int): Tuple2[List[Boolean], Int] =
      i match {
        case i if (i < 0) => (Nil, 0)
        case _ => {
          // We are somewhere in the middle of the elements list.
          // Can we add the current item <==>
          // is there enough capacity?
          if (weights(i) > remainingCapacity) {
            // No, we can't. Skip this element and continue searching:
            val (decisions, value) = knapsack(i - 1, remainingCapacity)
            (false :: decisions, value)
          } else {
            // Yes, we could add the current element.
            // But the total value might be better if we skip it
            // and add other items which follow.

            // The value if we skip this item:
            val (skippedDecisions, skippedValue) = knapsack(i - 1, remainingCapacity)
            // The value if we take this item:
            val (usedDecisions, usedValue) =
            knapsack(i - 1, remainingCapacity - weights(i))
            // Take the maximum out of both options:
            if (skippedValue > usedValue + values(i)) {
              (false :: skippedDecisions, skippedValue)
            } else {
              (true :: usedDecisions, usedValue + values(i))
            }
          }
        }
      }

    val (decisions, value) = knapsack(weights.size - 1, capacity)
    (items.toList zip decisions.reverse, value)
  }
}