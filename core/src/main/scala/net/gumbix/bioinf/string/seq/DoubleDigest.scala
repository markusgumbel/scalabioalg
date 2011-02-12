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

import collection.immutable.::
import collection.mutable.HashSet
import org.apache.commons.math.util.MathUtils.{factorial => fac}
import net.gumbix.util.{Logger, Permutation}

/**
 * Double digest algorithm with a very poor performance.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class DoubleDigest(a: List[Int], b: List[Int], ab: List[Int])
        extends Permutation[Int] with Logger {
  private val MAX_TICKS = 100000
  private var c = 0

  def printSolutions() {
    println()
    if (calc.size > 1) println(calc.size + " solutions:")
    println(calc.toList.sorted.mkString("\n"))
  }

  val calc = {
    val iter = fac(a.size) * fac(b.size) * fac(ab.size)
    val ticks = iter / MAX_TICKS
    logln("iterations:  " + iter + " (" + ticks + " ticks)")

    val set = new HashSet[String]
    for (testA <- perm(a); testB <- perm(b); testAB <- perm(ab)) {

      printTicks()

      // logln(testA + ", " + testB + ", " + testAB)
      val posA = pos(testA).drop(1).dropRight(1)
      val posB = pos(testB).drop(1).dropRight(1)
      val posAB = pos(testAB).drop(1).dropRight(1)
      // logln("Pos: " + posA + ", " + posB + ", " + posAB)

      val posABshould = (posA ::: posB).sorted

      // logln("--> " + posABshould)

      if (posABshould == posAB) {
        val line = posA.mkString("{", ", ", "}") + " + " +
                posB.mkString("{", ", ", "}") +
                " = " + posABshould.mkString("{", ", ", "}")
        set += line
      }
    }
    set
  }

  /**
   * Create a list with the positions given the ordered fragments size.
   */
  def pos(l: List[Int]) = {
    def append(v: Int, m: List[Int]): List[Int] = m match {
      case Nil => v :: Nil
      case h :: t => v :: append(h + v, t)
    }
    append(0, l)
  }

  private def printTicks() {
    if (c == MAX_TICKS) {
      log(".")
      System.out.flush
      c = 0
    }
    c += 1
  }
}