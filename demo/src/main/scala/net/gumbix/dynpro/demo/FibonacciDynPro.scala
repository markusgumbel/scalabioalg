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

/**
 * Fibonacci states.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object FibonacciState extends Enumeration {
  type FibonacciState = Value

  val INIT = Value("Init: i = 0 or 1")
  val NORMAL = Value("Normal: i > 1")
}

import net.gumbix.dynpro.demo.FibonacciState._
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.DynProMatrixPrinter
import net.gumbix.dynpro.DynPro
import net.gumbix.dynpro.PathEntry

/**
 * The Fibonacci numbers as dynamic programming problem.
 * @param k Compute the fib(k)
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class FibonacciDynPro(k: Int)
        extends DynPro[FibonacciState] with DynProMatrixPrinter[FibonacciState] {
  formatter = INT

  def n = k + 1

  /**
   * The matrix just has one column, i.e. it is a one dimensional
   * array.
   */
  def m = 1

  /**
   * If the first (idx.i = 0) or second (idx.i = 1) Fibonacci
   * is to be computed, the decision is INIT; otherwise it is NORMAL.
   */
  def decisions(idx: Idx) = {
    if (idx.i == 0 || idx.i == 1) Array(INIT)
    else Array(NORMAL)
  }

  /**
   * If the decision is INIT there are no previous states.
   * Otherwise there are two predecessors, namely the numbers
   * found at the relative position -1 and -2.
   */
  def prevStates(idx: Idx, d: FibonacciState) = d match {
    case INIT => Array()
    case _ => Array(Idx(idx.i - 1, 0), Idx(idx.i - 2, 0))
  }

  /**
   * If the decision is INIT, the value is the start value 1.
   * If not, the (added) value is 0 as the dynamic programming
   * algorithm adds the value of the two previous states:
   * a = 0 + fib(k-2) + fib(k-1)
   */
  def value(idx: Idx, d: FibonacciState) = d match {
    case INIT => 1
    case _ => 0
  }

  def solution: List[PathEntry[FibonacciState]] = solution(Idx(n - 1, 0))

  override def columnLabels = None
}