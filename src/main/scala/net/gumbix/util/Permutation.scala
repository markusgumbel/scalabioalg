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
package net.gumbix.util

/**
 * Copied from ADS version.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait Permutation[T] {
  /**
   * @return all permutations of the given list
   */
  def perm(list: List[T]): List[List[T]] = list match {
    case x :: Nil => List(List(x))
    case _ =>
      // Shift the list of size n n-times such that
      // each element is at the beginning. Store the shifted
      // lists in shiftedLists
      val shiftedLists = for (i <- 1 to list.size) yield {
        // Split the list at position 1 .. n
        val (a, b) = list splitAt i
        // Combine the splitted lists in reversed order:
        b ::: a
      }
      // Iterate through all shifted lists:
      val nestedPermutation = for (list <- shiftedLists.toList) yield {
        // Create the permuations of the remaining list:
        val perms = perm(list.tail)
        // And combine them with the head:
        val permutation = for (perm <- perms) yield {
          list.head :: perm
        }
        permutation
      }
      // Finally, make one long and flat list:
      nestedPermutation.flatten
  }
}