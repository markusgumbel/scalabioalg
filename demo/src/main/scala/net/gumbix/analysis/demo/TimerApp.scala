package net.gumbix.analysis.demo

import net.gumbix.analysis.FactoringMode._
import net.gumbix.analysis.DynPro._
import net.gumbix.analysis.Timer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/16/13
 * Time: 7:58 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object TimerApp{
  def main(args: Array[String]) {
    //runAnalysis(100, 1E20.asInstanceOf[Long], 10, GEO, GLOBALG, VITERBI)

    Timer.cleanResultDir
    Timer.runAnalysis(100, 2000, 20, 50, ARI, GLOBALG, VITERBI)

  }
}
