package net.gumbix.bioinf.struct.test

import junit.framework.TestCase
import net.gumbix.dynpro.Idx
import net.gumbix.bioinf.struct.NussinovCount

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class StructTest extends TestCase {

  //val strings = "AAABBAABBBB" :: "ABABBAABBBB" :: Nil
  //val strings = "AAABAAA" :: Nil
  // human tRNA
  // http://www.ebi.ac.uk/ena/data/view/M31637&display=text
  val strings = "UCCCUGGUGGUCUAGUGGDUAGGAUUCGGCGCUCUCACCGCCGCGGCCCGGGUUCGAUUCCCGGUCAGGGAACCA" :: Nil
  // AABBABBBABABBBABAAABBABABBABAB
  // AABBBAAAABBABBBABABBBABAAABBABABBABABAABBABBBABABBBABAAABBABABBABAB
  // AACBBCBBAAAACCAAABAABBABBACABBABBCBBA

  def test01() {
    strings.foreach(doStructure(_))
  }

  def doStructure(s: String) {
    println("\n---")
    val dp = new NussinovCount(s)
    val solution = dp.solution(Idx(0, dp.n - 1))
    solution.foreach(e => println(e.decision))
    println(dp.mkMatrixString(solution))
  }
}