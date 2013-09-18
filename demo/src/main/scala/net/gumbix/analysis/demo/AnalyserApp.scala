package net.gumbix.analysis.demo

import net.gumbix.analysis.Analyser
import net.gumbix.analysis.{DynPro, FactoringMode, AnalyserMode}
import DynPro._
import FactoringMode._
import AnalyserMode._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/16/13
 * Time: 7:58 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * @note To run this object make sure your maximum heap size has a value of at least Xmx1024m
 *
 */
object AnalyserApp{
  def main(args: Array[String]){
    //runAnalysis(100, 1E20.asInstanceOf[Long], 10, GEO, GLOBALG, VITERBI)

    Analyser.cleanResultDir
    Analyser.runWithRatio(1000, 100000, 20, 1000, 1.1, ARI, CON, VITERBI)

    //Analyser.run(100, 10000, 10, 500, 1/2, ARI, CON, VITERBI)
  }
}
