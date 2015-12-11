package net.gumbix.bioinf.hmm

/**
 * (c) 2012 by Markus Gumbel (m.gumbel@hs-mannheim.de)
 * @author Markus Gumbel
 */
class ProfileHMM(s: Array[Char], alphabet: Array[Char],
                 states: Array[Char],
                 transP: Array[Array[Double]],
                 emmP: Array[Array[Double]])
  extends Viterbi(s, alphabet, states, transP, emmP) {

  // def this() = this()
}
