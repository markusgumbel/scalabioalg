package net.gumbix.bioinf.string.alignment.test

import junit.framework.TestCase
import net.gumbix.bioinf.hmm.HiddenMarkovAutomata

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class RandomSequenceTest extends TestCase {
  // q0 and n(ormal)
  val transCoin = Array(Array(0.0, 1.0), Array(0.0, 1.0))

  val emmCoin = Array(Array(0.5, 0.5))

  val alphabetCoin = "10".toArray
  val statesCoin = Array('n')

  def testGenerateOutput() {
    val hmm = new HiddenMarkovAutomata(alphabetCoin, statesCoin,
      transCoin, emmCoin)
    val (states, chars) = hmm.makeString(100)
    print(chars.mkString(" "))
    println
  }
}