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
 * The Fibonacci numbers as dynamic programming problem.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

object FibonacciState extends Enumeration {
  type FibonacciState = Value

  val INIT = Value("Init: i = 0 or 1")
  val NORMAL = Value("Normal: i > 1")
}

import net.gumbix.dynpro.demo.FibonacciState._
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.MatrixPrinter
import net.gumbix.dynpro.DynPro
import net.gumbix.dynpro.PathEntry

class FibonacciDynPro(k: Int)
        extends DynPro[FibonacciState] with MatrixPrinter[FibonacciState] {
  formatter = INT

  def n = k + 1

  def m = 1

  def decisions(idx: Idx) = {
    if (idx.i == 0 || idx.i == 1) Array(INIT)
    else Array(NORMAL)
  }

  def prevStates(idx: Idx, d: FibonacciState) = d match {
    case INIT => Array()
    case _ => Array(Idx(idx.i - 1, 0), Idx(idx.i - 2, 0))
  }

  def value(idx: Idx, d: FibonacciState) = d match {
    case INIT => 1
    case _ => 0
  }

  def solution: List[PathEntry[FibonacciState]] = solution(Idx(n - 1, 0))

  override def columnLabels = None
}