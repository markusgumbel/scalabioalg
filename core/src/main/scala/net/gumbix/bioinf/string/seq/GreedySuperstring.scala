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
package net.gumbix.bioinf.string.seq

import collection.mutable.{ArrayBuffer, HashMap}
import net.gumbix.util.Logger

/**
 * Also, what do we do if we have more than one maximum?
 * Which one to take?
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
abstract class GreedySuperstring(s: Array[String]) extends Overlap {
  val superstrings: List[String]
}

/**
 * Imperative version of this algorithm according to
 * Böckenhauer & Bongartz, p. 175.
 */
class GreedySuperstringImperative(s: Array[String]) extends GreedySuperstring(s) {
  val superstrings = {
    // frgmts is reduced an reassembled until it contains one element:
    var frgmts = new ArrayBuffer[String]() ++ s
    while (frgmts.size > 1) {
      // Calculate all overlaps for all pairs (i, j) except i = j.
      val ovls = for (i <- 0 until frgmts.size; j <- 0 until frgmts.size;
                      if (i != j)) yield {
        val ov = overlap(frgmts(i), frgmts(j))
        (ov, i, j)
      }
      // Identify the pair with the maximum overlap:
      val (ov, i, j) = ovls.reduceLeft {
        (ovl1, ovl2) =>
          if (ovl1._1.size >= ovl2._1.size) ovl1 else ovl2
      }
      // println(w(i) + " _ " + w(j) + " = " + ov)
      // Merge these two fragments:
      val merged = frgmts(i).substring(0, frgmts(i).size - ov.size) + frgmts(j)
      // println("merged: " + merged)
      // Remove the fragments at position i and j from the array:
      val cleanFrgmnts = for (k <- 0 until frgmts.size; if (k != i && k != j))
      yield frgmts(k)
      frgmts = new ArrayBuffer() ++ cleanFrgmnts
      // ... and add the merged fragment:
      frgmts += merged
    }
    List(frgmts(0)) // TODO change alg.
  }
}

/**
 * Functional version of this algorithm according to
 * Böckenhauer & Bongartz, p. 175.
 */
class GreedySuperstringFct(s: Array[String]) extends GreedySuperstring(s)
        with Logger with SimpleSubstringFree {
  lazy val superstrings = {
    def buildString(frgmts: Array[String]): List[String] = frgmts.size match {
      case 0 => throw new RuntimeException("Fragments array must not be empty")
      case 1 => List(frgmts(0))
      case _ => {
        // Calculate overlaps for all pairs (i, j) except i = j.
        val ovls = for (i <- 0 until frgmts.size; j <- 0 until frgmts.size;
                        if (i != j)) yield {
          val ov = overlap(frgmts(i), frgmts(j))
          (ov, i, j)
        }
        // Identify the pair with the maximum overlap:
        val (ov, i, j) = ovls.reduceLeft {
          (ovl1, ovl2) =>
            if (ovl1._1.size >= ovl2._1.size) ovl1 else ovl2
        }
        // If overlap has size 0 we cannot overlap anymore.
        ov.size match {
          case 0 => {
            val nonOverlapping = frgmts(i) + "|" + frgmts(j)
            logln("Warning, non overlapping: " + nonOverlapping)
            frgmts.toList
          }
          case _ => {
            // Merge these two fragments:
            val merged =
            frgmts(i).substring(0, frgmts(i).size - ov.size) + frgmts(j)
            logln("'" + frgmts(i) + "' _ '" + frgmts(j) + "' = '" + merged +
                    "', ov = '" + ov + "'")
            // Remove the fragments at position i and j from the array:
            val cleanFrgmnts = for (k <- 0 until frgmts.size;
                                    if (k != i && k != j))
            yield frgmts(k)
            // ... and add the merged fragment:
            val newFrgmnts = Array.concat(cleanFrgmnts.toArray, Array(merged))
            buildString(newFrgmnts)
          }
        }
      }
    }
    val nonRedundant = substringFree(s)
    logln("\nNon redundant fragments:")
    logln(nonRedundant.mkString(", \n"))
    logln("")
    buildString(nonRedundant)
  }
}

trait Overlap {
  /**
   * Overlap in O(n&#94;2) in functional style.
   */
  def overlap(s1: String, s2: String): String = {

    def ov(s1: List[Char], s2: List[Char]): List[Char] = {

      /**
       * @param trail Overlap so far. The sequence is created in a
       * reverse order in order to simply add head at beginning of the list.
       */
      def pov(s1: List[Char], s2: List[Char], trail: List[Char]): List[Char] = {
        (s1, s2) match {
          case (Nil, _) => trail // end of s1 => trail is overlap
          case (_, Nil) => Nil // end of s2 => no overlap
          case _ if (s1.head != s2.head) => Nil // difference seq. => no overlap
          // Append s1 at end of trail
          case _ => pov(s1.tail, s2.tail, s1.head :: trail)
        }
      }

      val ovl = pov(s1, s2, Nil) // Calc. reverse overlap
      ovl match {
        case hd :: tl => ovl.reverse // Valid, reverse it
        // No valid overlap found, try to remove first character
        // of s1 and try again:
        case _ => s1 match {
          case hd :: Nil => Nil // No more characters in s1.
          case hd :: tail => ov(tail, s2)
          case _ => Nil // s1 is empty.
        }
      }
    }
    ov(s1.toList, s2.toList).mkString
  }
}

/**
 * Suboptimal (quick and dirty) hack to eliminate redundant substrings.
 */
trait SimpleSubstringFree {
  def substringFree(frgmts: Array[String]) = {
    val targetList = new ArrayBuffer[String]
    // Go over each string and test if another string contains it...
    for (s <- frgmts; if s.size > 0) {
      val subs = frgmts.filter(_.contains(s)) // All strings that contain s
      // If subs contains only 1 element this is s itself => unique string
      if (subs.size == 1) targetList += s
    }
    targetList.toArray
  }
}