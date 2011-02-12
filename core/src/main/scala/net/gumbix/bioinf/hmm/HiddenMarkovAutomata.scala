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

import collection.mutable.ArrayBuffer

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class HiddenMarkovAutomata(val alphabet: Array[Char], val states: Array[Char],
                           val transP: Array[Array[Double]],
                           val emmP: Array[Array[Double]], seed: Long) {
  def this(alphabet: Array[Char], states: Array[Char],
           transP: Array[Array[Double]],
           emmP: Array[Array[Double]])
  = this (alphabet, states, transP, emmP, System.currentTimeMillis)

  /**
   * @return A pair consisting of the states (Array of int) and
   * the emitted characters (Array of chars).
   */
  def makeString(size: Int): Tuple2[Array[Char], Array[Char]] = {

    val random = new java.util.Random(seed)

    /**
     * Calculate a next random index for a transition probability vector.
     * @param probs Consider the values in probs as probabilities.
     * @return Next index according to its probability.
     */
    def nextIndex(probs: Array[Double]): Int = {
      val accProbs = new ArrayBuffer[Double]()
      // Accumulate all probabilities. E.g. if all six p=1/6
      // then accProcs = (1/6, 2/6, ..., 6/6)
      probs.foreach {
        it =>
          if (accProbs.isEmpty) {
            accProbs += it
          } else {
            accProbs += accProbs(accProbs.size - 1) + it
          }
      }
      // Now draw a random number from [0; 1]
      val r = random.nextFloat
      for (i <- 0 until probs.size) {
        if (r < accProbs(i)) {
          return i
        }
      }
      return probs.size - 1 // Should not happen
    }

    val stateList = new ArrayBuffer[Char]()
    val charList = new ArrayBuffer[Char]()
    var state = 0
    for (i <- 0 until size) {
      // Get the next transition state:
      state = nextIndex(transP(state))
      stateList += states(state - 1)
      // Get the next emmission state:
      val charIdx = nextIndex(emmP(state - 1))
      val c = alphabet(charIdx)
      charList += c
    }
    (stateList.toArray, charList.toArray)
  }
}