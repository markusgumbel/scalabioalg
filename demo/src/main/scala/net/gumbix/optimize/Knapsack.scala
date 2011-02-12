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

import net.gumbix.layout.Element._
import net.gumbix.layout.Element

/**
 * Some features of the knapsack problem.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait Knapsack {

  /**
   * A list of names for items to carry.
   */
  val items: Array[String]

  /**
   * The weights of each item.
   */
  val weights: Array[Int]

  /**
   * The value of each item.
   */
  val values: Array[Int]

  /**
   * How many can I carry?
   */
  val capacity: Int

  /**
   * The solution of the knapsack problem.
   */
  def knapsackSolution: Array[Int]

  /**
   * Create a table with all relevant information.
   */
  def mkOverviewString = {

    def mkColumn(list: List[Any]): Element = list match {
      case h :: Nil => line(h.toString)
      case h :: t => line(h.toString) above mkColumn(t)
    }
    val iColumn = line("i") above expandableLine("-", '-') above
            mkColumn((0 until weights.size).toList)
    val aColumn = line("a_i") above expandableLine("-", '-') above
            mkColumn(items.toList)
    val wColumn = line("w_i") above expandableLine("-", '-') above
            mkColumn(weights.toList)
    val vColumn = line("v_i") above expandableLine("-", '-') above
            mkColumn(values.toList)
    val sColumn = line("d_i") above expandableLine("-", '-') above
            mkColumn(knapsackSolution.toList)

    iColumn beside expandableLine("|", '|') beside
            aColumn beside expandableLine("|", '|') beside
            wColumn beside expandableLine("|", '|') beside
            vColumn beside expandableLine("|", '|') beside
            sColumn
  }
}