package net.gumbix.analysis.demo

import net.gumbix.analysis.FactoringMode._
import net.gumbix.analysis.DynPro._
import net.gumbix.analysis.Analyser

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
    Analyser.run(100, 200, 20, 25, ARI, GLOBALG, VITERBI)
  }
}
