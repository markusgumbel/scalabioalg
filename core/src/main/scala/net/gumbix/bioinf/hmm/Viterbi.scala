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
package net.gumbix.bioinf.hmm

import net.gumbix.dynpro.Backpropagation
import net.gumbix.dynpro.CellPosition._
import net.gumbix.dynpro.{Idx, DynPro, MatrixPrinter}
import collection.mutable.ArrayBuffer
import scala.math.log

/**
 * The Viterbi algorithm to determine patterns in a string.
 * @param s The string to analyse.
 * @param alphabet Alphabet of the emissions.
 * @param states States (excl. q0).
 * @param transP Transition-Probabilities, e.g. probability from
 * going from state p to q.
 * @param emmP Emission-Probabilities, e.g. probability to emit
 * the character c when being in state q. 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class Viterbi(val s: StringBuilder, val alphabet: Array[Char],
              val states: Array[Char],
              val transP: Array[Array[Double]],
              val emmP: Array[Array[Double]])
        extends DynPro[Int]
                with Backpropagation[Int]
                with MatrixPrinter[Int] {

  /**
   * Values can become very small, so a scientific notation is required.
   */
  formatter = ENGINEER

  override val backpropagationStart = MAXIMUM_VALUE_LAST_ROW

  override def updateXY(newS: String, __ : String){s.append(newS)}
  def updateS(newS: String){updateXY(newS, "")}


  /**
   * Length of the string to analyse plus the empty string (-).
   */
  def n = s.length + 1

  /**
   * Number of states
   */
  def m = states.length

  /**
   * Decisions are the states (incl. q0)
   */
  def decisions(idx: Idx) = {
    if (idx.i == 0) (0 to 0).toArray
    else (1 to states.length).toArray
  }

  def prevStates(idx: Idx, d: Int) =
    if (idx.i > 0) Array(Idx(idx.i - 1, d - 1)) else Array()

  def value(idx: Idx, dState: Int) = (idx.i, idx.j) match {
    case (0, jState) => log(transP(0)(jState + 1))
    case (iChar, jState) => {
      // The index of the current char (for this row):
      val idxS = alphabet.indexOf(s.charAt(iChar - 1))
      val e = emmP(jState + 1 - 1)(idxS) // state, char
      val t = transP(dState)(jState + 1)
      log(e * t)
    }
  }

  override def rowLabels: Array[String] = makeLabels(s.toArray, ".")

  private def makeLabels(s: Array[Char], first: String) = {
    val buffer = new ArrayBuffer[String]()
    buffer += first
    s.foreach(buffer += _.toString)
    buffer.toArray
  }
}