package net.gumbix.analysis.demo

import net.gumbix.analysis.Timer
import net.gumbix.analysis.FactoringMode._
import net.gumbix.analysis.DynPro._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/16/13
 * Time: 7:58 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object TimerApp extends Timer(100){
  def main(args: Array[String]) {
    runAnalysis(100, 1E20.asInstanceOf[Long], 10, GEO, GLOBALG)
    runAnalysis(100, 1E20.asInstanceOf[Long], 10, GEO, VITERBI)
  }
}
