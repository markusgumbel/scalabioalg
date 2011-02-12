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

import net.gumbix.optimize.Knapsack
import net.gumbix.dynpro.{Idx, MatrixPrinter, DynPro, PathEntry}
/*
i|a_i|w_i|v_i|d_i
-|---|---|---|---
0| A | 5 | 4 | 0
1| B | 6 | 5 | 1
2| C | 2 | 3 | 1
3| D | 2 | 6 | 1

   |  0    1   2    3  4   5  6  7  8  9 10
- -|---- --- ---- --- --- -- -- -- -- -- ---
0 A| 4.   4.  4.   4. 4.  4. 0. 0. 0. 0. 0.*
1 B| 5.   5.  5.   5. 5.* 4. 0. 0. 0. 0. 0.
2 C| 8.   8.  8.*  7. 5.  4. 3. 3. 3. 0. 0.
3 D|14.* 13. 11.  10. 9.  9. 9. 6. 6. 0. 0.

decision=0, v=0.0, curr=(0, 10), prev=none
decision=1, v=5.0, curr=(1, 4), prev=Array((0, 10))
decision=1, v=8.0, curr=(2, 2), prev=Array((1, 4))
decision=1, v=14.0, curr=(3, 0), prev=Array((2, 2))
*/

/**
 * A version of the Knapsack problem that I do understand :-).
 * This variant builds the matrix by going forward and then
 * it calculates the decision path by going backwards.
 * This approach is the same like in string alignments.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class KnapsackDynPro(val items: Array[String],
                      val weights: Array[Int], val values: Array[Int],
                      val capacity: Int)
        extends DynPro[Int]
                with MatrixPrinter[Int]
                with Knapsack {
  formatter = INT

  def n = weights.length

  def m = capacity + 1

  /**
   * If the remaining capacity (idx.j) plus the weight that could be taken
   * is less than the overall capacity we could take it. Thus,  { 0, 1 }.
   * If not, we can only skip it (={0}).
   */
  def decisions(idx: Idx) = {
    if (idx.j + weights(idx.i) <= capacity) Array(0, 1)
    else Array(0)
  }

  /**
   * The prev. state is the previous item (idx.i-1) and the prev. capacity.
   * The prev. capacity is the remaining capacity (idx.j) plus weight that was
   * taken (or plus 0 if it was skipped).
   */
  def prevStates(idx: Idx, d: Int) = {
    if (idx.i > 0) Array(Idx(idx.i - 1, idx.j + d * weights(idx.i)))
    else Array()
  }

  /**
   * @param idx The state (idx.i = index to i-th item, idx.j = capacity)
   * @param d Decision which is either 0 (skip the i-th item) or
   * 1 (take it). 
   */
  def value(idx: Idx, d: Int) = d * values(idx.i)

  /**
   * The maximum is expected at the last item (n-1) with no capacity left (0).
   */
  def solution: List[PathEntry[Int]] = solution(Idx(n - 1, 0))

  override def rowLabels: Array[String] = items

  override def columnLabels = None

  def knapsackSolution = solution.map(_.decision).toArray // from Knapsack
}