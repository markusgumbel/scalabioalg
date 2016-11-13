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
package net.gumbix.bioinf.hmm.demo

import net.gumbix.bioinf.hmm.{HiddenMarkovAutomata, Viterbi}
import org.apache.commons.math3.stat.Frequency
import org.junit.{Ignore, Test}

/**
 * Demo classes using th Viterbi-algorithm.
 * 1. a unfair casino cube and 2. identifying CpG islands.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class HMMDemo {

  /**
   * Run a hidden markov model.
   */
  def hmm(s: Array[Char], a: Array[Char], q: Array[Char],
          transP: Array[Array[Double]], emmP: Array[Array[Double]]) {
    val dp = new Viterbi(s, a, q, transP, emmP)
    val solution = dp.solution

    val sol = solution.slice(1, solution.length)
    val path = sol.map(e => q(e.decision - 1))
    path.foreach(print _)

    println()
    println(dp.mkMatrixString(solution))
    println("")
  }

  // --- Unfair casino cube example ---

  val transPCube = Array(
    // q0, Fair, Unfair:
    Array(0, 0.5, 0.5),
    Array(0, 19.0 / 20.0, 1.0 / 20.0),
    Array(0, 1.0 / 20.0, 19.0 / 20.0)
    )

  val emmPCube = Array(
    // Fair:
    Array(1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0),
    // Unfair:
    Array(1.0 / 10.0, 1.0 / 10.0, 1.0 / 10.0, 1.0 / 10.0, 1.0 / 10.0, 1.0 / 2.0)
    )

  val alphabetCube = "123456".toArray
  val statesCube = Array('F', 'U')

  /**
   *  A unfair casino cube.
   */
  @Test
  @Ignore
  def testCasinoDice() {
    // A series of dices:
    val seq = "62231536341315646366643554665521346665366662653152146451645155142433221646165432521555433525564463164362443113253656263634162164646166664666335356141561615546641221146336656534225656666661664646553246661666145523656326662215261166424346413143653613665436154535624642435335626561342254661453662516143543633616256646611663523342426164146666614126664165246665542544213355511426625646645413443656346652413565353666332666662653666366253636645666645666165526434466434465351111221411466464423316135345662264"

    hmm(seq.toArray, alphabetCube, statesCube, transPCube, emmPCube)
  }

  @Test
  @Ignore
  def testCompareRealVsCalculatedCube() {
    val hmm = new HiddenMarkovAutomata(alphabetCube,
      statesCube, transPCube, emmPCube, 2)

    val (states, chars) = hmm.makeString(100)

    val statesHisto = new Frequency()
    states.foreach(statesHisto.addValue(_))
    println("Frequency states:")
    statesCube.foreach(s => println(s + " = " + statesHisto.getPct(s)))

    val charHisto = new Frequency()
    chars.foreach(charHisto.addValue(_))
    println("Frequency characters:")
    alphabetCube.foreach(s => println(s + " = " + charHisto.getPct(s)))

    val dp = new Viterbi(chars, alphabetCube, statesCube, transPCube, emmPCube)
    val solution = dp.solution
    // println "details = $dp.fullSolution"

    /*
    println()
    println("Emmitted Chars:  " + chars.mkString)
    println("Real States:     " + states.mkString)
    */
    val sol = solution.slice(1, solution.length)
    val path = sol.map(e => statesCube(e.decision - 1))
    /*
    println("Calc. States:    " + path.mkString)
    println()
    */

    var err = 0
    for (i <- 0 until states.length) {
      if (states(i) != path(i)) err += 1
    }
    val er = err.asInstanceOf[Double] / states.length
    println("e = " + er + ", " + err + " of " + states.length + " are wrong.")

    println()
    println(dp.mkMatrixString(solution))
  }

  // --- CpG island example ---

  val transPCG2 = Array(
    // q0, 'a', 't', 'c', 'g', 'A', 'T', 'C', 'G'
    Array[Double](0, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f),
    Array(0, 0.15, 0.26, 0.19, 0.10, 0.06, 0.11, 0.08, 0.04),
    Array(0, 0.13, 0.26, 0.21, 0.10, 0.05, 0.11, 0.09, 0.04),
    Array(0, 0.14, 0.16, 0.25, 0.14, 0.06, 0.07, 0.11, 0.06),
    Array(0, 0.07, 0.26, 0.22, 0.15, 0.03, 0.11, 0.10, 0.06),
    Array(0, 0.03, 0.03, 0.02, 0.02, 0.27, 0.25, 0.19, 0.19),
    Array(0, 0.03, 0.03, 0.02, 0.02, 0.23, 0.29, 0.21, 0.17),
    Array(0, 0.03, 0.01, 0.03, 0.03, 0.29, 0.08, 0.28, 0.25),
    Array(0, 0.02, 0.03, 0.02, 0.03, 0.17, 0.28, 0.22, 0.23)
    )
  val transPCG = Array(
    // q0, 'a', 't', 'c', 'g', 'A', 'T', 'C', 'G'
    Array[Double](0, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f, 1 / 8f),
    Array(0, 0.13, 0.23, 0.16, 0.09, 0.08, 0.15, 0.11, 0.06),
    Array(0, 0.11, 0.22, 0.18, 0.09, 0.07, 0.15, 0.12, 0.06),
    Array(0, 0.12, 0.14, 0.22, 0.12, 0.08, 0.09, 0.14, 0.08),
    Array(0, 0.06, 0.22, 0.19, 0.13, 0.04, 0.15, 0.13, 0.09),
    Array(0, 0.12, 0.11, 0.08, 0.08, 0.18, 0.17, 0.13, 0.13),
    Array(0, 0.10, 0.13, 0.09, 0.08, 0.15, 0.19, 0.14, 0.11),
    Array(0, 0.13, 0.03, 0.12, 0.11, 0.19, 0.05, 0.19, 0.17),
    Array(0, 0.07, 0.12, 0.10, 0.10, 0.11, 0.19, 0.15, 0.15)
    )

  val emmPCG = Array(
    // a:
    Array(1.0, .0, .0, .0),
    // t:
    Array(.0, 1.0, .0, .0),
    Array(.0, .0, 1.0, .0),
    Array(.0, .0, .0, 1.0),
    // A:
    Array(1.0, .0, .0, .0),
    // T:
    Array(.0, 1.0, .0, .0),
    Array(.0, .0, 1.0, .0),
    Array(.0, .0, .0, 1.0)
    )

  val alphabetCG = "ATCG".toArray
  val statesCG = Array('a', 't', 'c', 'g', 'A', 'T', 'C', 'G')

  /**
   * Run the viterbi algorithms to find CpG-island.
   * Note: Requires sufficient stack size, e.g. -Xss10M
   */
  @Test
  @Ignore
  def testCpG() {
    val s = "ACTCTCTCCTCCTCCTCACCTCATTGTCTCCCCGACTTATCCTAATGCGAAATTGGATTCTGAGCATTTGTAGCAAAATCGCTGGGATCTGGAGAGGAAGACTCAGTCCAGAATCCTCCCAGGGCCTTGAAAGTCCATCTCTGACCCAAAACAATCCAAGTAAGTACCTAATTCCTTTGGGAGTGGGTTGTGTATCTCACAGCAACAGAGAAAAAATAGTCACTTAAAAGTTTCTCTTTGACATCTGTAATGTATGTCAATAAATGAATTCTAAGTTAGTAGAGTTTGATGTAAAGTCCTGAAAATTAAAAAAGAGAGAAACTAAAAAACAAAAAGAAGCAGAAGCAAAAGTTAATGAGTCTTAACAGTTGCTTACCTATTGAAAACTTATTTAGAAATACTCTTTTAACATTGTGGTCACCTGAGTAAATCACTGGAGATAGTGCATTTCAGAAATGTCTCCGTTCTGATTCCATAAACAATTTGACTTGTATAGTGTGCTATATTTTGGTGATTTATCAAATCTTGATGTGAGTTTGGGAGTATTGCTAATGTCAGATGACTTGGGAACTAAGAATAAGACATTTAACCTATGCTTAATTGAAATGAAATTTTTCCCTAGAAGAAGAGTAGGTGGAAAAAGTCTTCTTTCTTGACTTCAGTTGTAAACTCTTCTATTGCTTTCCATTTTGAATATTAATATGACAGGAAATATCAGATGGAAATATTTTTAAAAGATAGAAATGTGAGTATGACGAAGAACTTTAGTAATAAAATTGTCCAAGGACTAAATTTATAGATAAGATACCTCTTTGTCTCCTTATTGACAGAGTGAATGGGGCAACTGTGGAGCCTGACTTACTTCTTTTAATTGGGTTTTTATTCAGAAGGGAGGGGCAGGAGGGAATGACAAGTGACTCACCTTGAATTCTTCCTCTAAGAAACTCACACCTGAGCTTTGAGCTATAAAGAAATCTGATGCTGTTTCTGGTGCTGTCTTAGAATCACTTCAGGAGTATTGACAAGAGGGGTAGGAACCCTTAGAAATAATATTAGTGATAAATAAGAAGGCAGGAAGAAACTTTTGGAGGTGATGGATAGGTTTATGGTATAGATTGTGGTGATGATTTAATGAGTGTATGCCTATCCCCAGACTCATCAAAGTGTATACATGGAATATGTAAAGCTTTTATATGTCAGTCACACCTCAGTAAAGTGGTTTACCTATCTATCTATCTATCTATCTATCTATCTATCTAAATTTTTTTTTCTGTTCCTAAAAAAAGGAAGGGAGAAGAGAGGAAAAGATGTTCAGGGAGCTACCATTTTGTTTCTAGCTGTGATTTTATAAAATGATAGACACTTTTATCTTTGTGTTACGTTCCTACCCCCAGTCCTCCAAATTATGGATCTGTGCCATTTGTACCGTGGACTTTTCTGTTTTCTGAGGATGTTGCAACAAATACTGATGCAACTCCTGGTTAACTGATAAAGTACTGGCCAGGGACAAAGCTCTCTTGTCCTGAGACCCTTCCTCAAGATTTGCAGCAATTTCCCACCACGTACCTCTGCCCTCTCCTCACAGCTGGAGAGGGAAAGTCATGGAATCCTTGTCCTTCCTCTTGTTTCCACCTCTTCAAGATTGGGCCAATTGCAATGGAATATCCATTGGTTGTGAGGCCTTTGTACTCTGCAAGGAAAAGAAAAGAAATGTGTGTATGTATGAGTGTGTGATGGAGCTAACTTTTCTACAATGTCTACTAACATGTCCTAGCCTTTACTTCATTCGCCTGTTTCCTTCTCACAAAAACCCTGTATGGGAGTTTTTCTTTACTTTTTATTATTATTTTTTTGAGACAAAGTCTCGCTCTGTCTCCCAGGCTGGAGTGCAGTGGCGCTATATCGGCTCACTGCAGCCTCCACCTCCCGGGTTCAAGCGATTCTCCTGCCTCAGCCTCCTGAGTAGCTGGTACTACAGGCGTGCACCACCATGCCACTATTTTTTGTATTTTTAGTAGAGACGGGGTTTCACTATGTTGGCCAGACTGGTCTCGAACTCTTGACCTCAGGTGATCCGCCCGCCTCGGCTTCCCAGAGTGCTAGGATTACAGGCGTGAGCCACTGCGCCCAGCCAGGAGTTTTTCTTATACTCATTTTACAGATGAGAAAACTGAGACTCAAAAAATACAAGTGACCCGTCCACAGGCAGATAGTTAGGAAGTAGCGGGACCTGAACTTGAGGGCGGGTCTTTCTGACTCCAAAGCCTCTTCCTGGCTACTCTGATATTGGCTATTGGCGGAGGCTGGGAAAACTTGAAATGGGGAATGATCGGGGAGCGGCGAGGGGGGACCAGCCGTTAAGCATTCCAGCCTGACAGGGGTGATTTGTTAAACCCAGGAACTAGTTAGACGTTTCCTGAAACCTCCTGCATAGGGCATTTTCGAGAGATTGCACCATCA"
    hmm(s.toArray, alphabetCG, statesCG, transPCG, emmPCG)
  }
}