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

import net.gumbix.dynpro.{PathEntry, DynProMatrixPrinter, Idx, DynPro}
import net.gumbix.optimize.Knapsack

/*
i|a_i|w_i|v_i|d_i
-|---|---|---|---
0| A | 5 | 4 | 0
1| B | 6 | 5 | 1
2| C | 2 | 3 | 1
3| D | 2 | 6 | 1

   | 0  1  2   3  4   5  6   7   8   9  10
- -|-- -- --- -- --- -- -- --- --- --- ----
0 A|0. 0. 6.  6. 9.  9. 9. 10. 11. 13. 14.*
1 B|0. 0. 6.  6. 9.  9. 9.  9. 11. 11. 14.*
2 C|0. 0. 6.  6. 9.* 9. 9.  9.  9.  9.  9.
3 D|0. 0. 6.* 6. 6.  6. 6.  6.  6.  6.  6.

decision=0, v=14.0, curr=(0, 10), prev=Array((1, 10))
decision=1, v=14.0, curr=(1, 10), prev=Array((2, 4))
decision=1, v=9.0, curr=(2, 4), prev=Array((3, 2))
decision=1, v=6.0, curr=(3, 2), prev=none
 */

/**
 * This variant builds the matrix by going backwards and then
 * it calculates the decision path by going forward.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class KnapsackDynProBackwards(items: Array[String],
                              weights: Array[Int], values: Array[Int],
                              capacity: Int)
        extends KnapsackDynPro(items, weights, values, capacity) {

  /**
   * Inverse the order of processing.
   * @param k is in range 0 to cellsSize-1.
   */
  override def getCellIndex(k: Int) = {
    val row = k / m
    val col = k % m
    Idx(n - 1 - row, m - 1 - col)
  }

  override def matrixForwardPathBackward = false

  override def decisions(idx: Idx) = (idx.i, idx.j) match {

  // If the remaining capacity is less
  // than the current weight, we can only skip it:
    case (i, rc) if (rc < weights(i)) => Array(0)

    // All options are possible:
    case _ => Array(0, 1)
  }

  override def prevStates(idx: Idx, d: Int) = {
    if (idx.i < n - 1) Array(Idx(idx.i + 1, idx.j - d * weights(idx.i)))
    else Array()
  }

  // Same as in superclass, kept for better reading:
  override def value(idx: Idx, d: Int) = d * values(idx.i)

  override def solution = solution(Idx(0, m - 1))
}