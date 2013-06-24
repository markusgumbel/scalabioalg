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
object TimerApp {

  def main(args: Array[String]) {
    val timer = new Timer(50, 1E5.asInstanceOf[Long], 10, ARI, 2)
    timer.runAnalysis(GLOBALG)
  }
}
