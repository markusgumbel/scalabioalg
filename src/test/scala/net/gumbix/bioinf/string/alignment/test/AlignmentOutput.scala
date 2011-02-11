package net.gumbix.bioinf.string.alignment.test

import net.gumbix.bioinf.string.alignment.AlignmentMode._
import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait AlignmentOutput {
  def doAligmentDP(mode: AlignmentMode, s1: String, s2: String, comment: String) {
    println()
    println("---------------------------------")
    println("Alignment: method = " + mode + "; " + comment)
    println()
    println("s1 = " + s1 + ", s2 = " + s2)
    val dp = new Alignment(s1, s2, mode)
    val solution = dp.solution
    println("sim = " + dp.similarity)
    println()
    println(dp.makeAlignmentString(solution))
    println()
    if (s1.size < 100 && s2.size < 100) {
      println(dp.mkMatrixString(solution))
      println()
      solution.foreach(e => print(e.decision))
      // solution.foreach(e => println(e))
      println()
    }
  }
}