package net.gumbix.bioinf.struct.demo

import junit.framework.TestCase
import net.gumbix.dynpro.Idx
import net.gumbix.bioinf.struct.NussinovCount

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class Struct2DDemo extends TestCase {
  /**
   * human tRNA
   * http://www.ebi.ac.uk/ena/data/view/M31637&display=text
   */
  val tRNA = "UCCCUGGUGGUCUAGUGGDUAGGAUUCGGCGCUCUCACCGCCGCGGCCCGGGUUCGAUUCCCGGUCAGGGAACCA"

  def testHumantRNA() {
    doStructure(tRNA)
  }

  def doStructure(s: String) {
    println("\n---")
    val dp = new NussinovCount(s)
    val solution = dp.solution(Idx(0, dp.n - 1))
    solution.foreach(e => println(e.decision))
    println(dp.mkMatrixString(solution))
  }
}