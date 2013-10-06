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
 * @note To conRun this object make sure your maximum heap size has a value of at least Xmx1024m
 *
 */
object AnalyserApp{
  def main(args: Array[String]){

    //Analyser.cleanResultDir
    //Analyser.conRun(50000, 1000, 10, 5000, 1.1, ARI, CON, VITERBI)
    //Analyser.conRun(100000, 100, 10, 2000, 2, ARI, CON, VITERBI)
    Analyser.seqRun(100, 10000, 10, 500, ARI, SEQ, VITERBI)
    Analyser.startMatLab

    /* This is as far as i can go, when it comes to the Global Alignment
     * Analyser.conRun(100, 2500, 20, 200, 1.1, ARI, CON, GLOBALG)
     */

    /* HERE AN SOME ADDITIONAL EXAMPLES
     * Keep in mind that your memory size has to be higher than usual that of usual laptops / personal computers
     * to perform these tasks.
     *
     * Analyser.conRun(1000, 1000000, 20, 5000, 1.1, ARI, CON, VITERBI, GLOBALG)
     * Analyser.conRun(100, 1E20.asInstanceOf[Long], 20, 1.1, GEO, SEQ, GLOBALG, VITERBI)
     *  - more than one dyn pro alg can be tested at once (concurrently or sequentially)
     *
     * Analyser.conRun(1000, 1000000, 20, 5000, -1, ARI, SEQ, VITERBI, GLOBALG)
     *  - This analysis will be performed with the highest amount of actors possible.
     *
     * Analyser.conRun(100, 10000, 10, 500, 1/2, ARI, CON, VITERBI)
     * - Between 100 and 11000/2 the computation will be seemly sequential and
     *   between 11000/2 and 10000 fully concurrent.
     *
     * Analyser.conRun(1000, 1000000, 20, 5000, 30000, ARI, SEQ, GLOBALG)
     * - Between 100 and 30000 the computation will be seemly sequential and
     *   between 30000 and 10000 fully concurrent.
     *
     * Analyser.seqRun(100, 10000, 10, 500, ARI, CON, VITERBI)
     */
  }
}
