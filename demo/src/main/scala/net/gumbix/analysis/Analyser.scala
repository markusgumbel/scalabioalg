package net.gumbix.analysis

import scala.collection.mutable.{ListBuffer, Map}

import java.io.File

import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Messages
import Messages._
import DynPro._
import FactoringMode._
import AnalyserMode._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 7:11 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * ********** DEFINITIONS **********
 * dyn pro alg =: dynamic programming algorithm
 * Round
 *  This term is used to defined the time required for the "Analyser" object (actually the "run" method)
 *  to test and get the results of a given sequence length.
 * Analysis
 *  The comparison of the performance between sequential- and concurrent modes over a range of sequence lengths.
 * 
 * ********** PERFORMANCE **********
 * To boast computation speed of during the stress test the Analyser object has been equipped
 * with 2 concurrency layers nested within one another.
 *
 * - The primary layer level is realised by the actor called "AnalyserActor".
 *   It the required number of computations per round per sequence length per dyn pro alg
 *   by creating a "DpSingleActor" for each of them.
 * - The secondary layer is provided the by the adequate DynProRunner extender method invoked.
 *   The sequential- and concurrent solutions are concurrently computed.
 *
 * ********** HOW TO ADD A dyn pro alg **********
 * To include an additional algorithm follow these steps.:
 * 1- Go to net.gumbix.analysis.DynPro (residing in net.gumbix.analysis.Others.scala) and
 *    an identifier (value) for the new algorithm.
 * 2- Go to net.gumbix.analysis.DynProRunner and create the sequential- and concurrent classes
 *    extending the new DynPro class and the method running them.
 *    - The classes should ideally only receive the sequences as parameters.
 *      They are conventionally named after the original DynPro class with the prefixes "Seq" and "Con".
 *    - The method can only receive the required sequence length and has to return the computation
 *      duration of both classe as a tupel. An adequate internal method is provided for this matter: "getDurations".
 *      It is conventionally named after the original DynPro class with the prefix "run".
 * 3- Within the "match" block of the net.gumbix.analysis.AnalyserActor.getTimeMap method,
 *    create case handling the identifier created in step 1 by simply invoking the method created in step 2.
 * 4- One can now include the new identifier in the net.gumbix.analysis.demo.AnalyserApp and run it.
 *
 *
 * @note For test purposes i have raised my
 *       - JAVA Maximum heap size to Xmx4096m and
 *       - IntelliJ Maximum heap size to Xmx1024m
 *       That's as far as i can go.
 */
protected[analysis] object Analyser{

  /**
   * The method raises the current sequence length.
   * @param mode see the "run" method
   * @param len The current sequence length
   * @param factor see the "run" method
   * @return
   */
  private def raiseCurLen(mode: FactoringMode, len: Long, factor: Int): Long = mode match{
    case ARI => len + factor
    case GEO => len * factor
    case EXP => math.pow(len, factor).asInstanceOf[Long]
  }

  /**
   * This method is used to get the set of all sequence lengths to be computed.
   * @return ListBuffer[Long](100, 150,..., 20000)
   */
  private def getLens(minLen: Int, maxLen: Long, leftRightBorder: Double, mode: FactoringMode, factor: Int) = {
    val (lens, lensLeft, lensRight) = (ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long]())
    var (lastRound, keepWhileLoopAlive, len) = (false, true, minLen.asInstanceOf[Long])

    while(keepWhileLoopAlive){
      lens += len //x-coordinates
      //println(leftRightBorder)
      if(len < leftRightBorder) lensLeft += len else lensRight += len //x-coordinates used by the DpActors

      //raising the sequence length
      if(lastRound) keepWhileLoopAlive = false
      else{
        len = raiseCurLen(mode, len, factor)
        if(len >= maxLen){
          len = maxLen
          lastRound = true
        }
      }
    }

    (lens, lensLeft, lensRight)
  }

  /**
   * The method is invoked to start the analysis.
   * @param minLen The minimum length
   * @param maxLen The maximum length
   * @param nrOfCom The number of computations per round
   * @param factor The sequence length expansion factor
   * @param fMode The sequence length expansion mode
   * @param aMode The analyser running mode
   * @param dynPros The dyn pro alg's used to run the analysis.
   * @return True (SUCCESSFUL) if the analysis is successful and all results have been successfully saved,
   *         false (ERROR) otherwise.
   */
  private def run(
    minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int,
    leftRightBorder: Long, fMode: FactoringMode, aMode: AnalyserMode, dynPros: Seq[DynPro]
  ) = if(minLen >= 100 && maxLen > minLen && nrOfCom >= 10 && factor > 0
    && fMode.isInstanceOf[FactoringMode] && aMode.isInstanceOf[AnalyserMode] && dynPros.isInstanceOf[Seq[DynPro]]){
    val lens = getLens(minLen, maxLen, leftRightBorder, fMode, factor)
    val options = "{range = %s ... %s | nrOfCom = %s | factor = %s | factoring mode = %s | nrOfActors = %s |%s DynPro(s) = %s} =: "
    .format(
      minLen, maxLen, nrOfCom, factor,
      fMode, 1 + 2*nrOfCom * (lens._3.length + 1),
      //nrOfActors =: Analyser + (nrOfCom {for lensLeft} + nrOfCom*lensRight) *2 {sequential, concurrent}
      if(dynPros.length > 1) " analyser mode = %s | " else "",
      dynPros.mkString(", ")
    )

    println(anaDate + options + "START!") //this is the 1st of 5 milestones
    val results = aMode match{
      case CON =>
        val actor = new AnalyserActor(minLen, maxLen, lens, nrOfCom, factor, fMode, dynPros)
        actor.start
        actor !? START match{case results: Map[DynPro, String] => results}

      case SEQ =>
        val results = Map[DynPro, String]()
        dynPros.map{dp =>
          val actor = new AnalyserActor(minLen, maxLen, lens, nrOfCom, factor, fMode, Seq(dp))
          actor.start
          actor !? START match{case _results: Map[DynPro, String] =>
            val key = _results.keySet.head
            results(key) = _results(key)
          }
        }
        results
    }
    println(anaDate + options + "END!\n\nResults: " + results.mkString(", "))
      //this is the 5th and last milestone.

    results
  }else{
    val (prefix, report) = ("\n\t", new StringBuilder("[%s] The Analyser couldn't start.".format(anaDate)))
    def updateReport(text: String){report ++= prefix + text + "."}

    if(minLen < 100) updateReport("The minimum sequence length is 100")
    if(maxLen <= minLen) updateReport("The maximum sequence length must be higher than the minimum sequence length")
    if(nrOfCom < 10) updateReport("The minimum number of computation per round is 10")
    if(!fMode.isInstanceOf[FactoringMode]) updateReport("The factoring mode is of type FactoringMode")
    if(!aMode.isInstanceOf[AnalyserMode]) updateReport("The analyser mode is of type AnalyserMode")
    if(!dynPros.isInstanceOf[Seq[DynPro]]) updateReport("The dynPros object is of type Seq[DynPro]")

    System.err.println(report)
    null
  }

  def runWithRatio(
    minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, ratio: Double,
    fMode: FactoringMode, aMode: AnalyserMode, dynPros: DynPro*
  ): Map[DynPro, String] = run(
    minLen, maxLen, nrOfCom, factor,
    (minLen + (maxLen - minLen) * ratio).asInstanceOf[Long],
    fMode, aMode, dynPros
  )

  def runWithBorder(
    minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, leftRightBorder: Long,
    fMode: FactoringMode, aMode: AnalyserMode, dynPros: DynPro*
  ): Map[DynPro, String] = run(
    minLen, maxLen, nrOfCom, factor, leftRightBorder, fMode, aMode, dynPros
  )


  /**
   * This method is used to the clear the result directory.
   * @return true if all files in the result directory have been successfully deleted
   *         otherwise false.
   */
  def cleanResultDir: Boolean = try{
    new File(dir).listFiles.map(f => f.delete).reduceLeft((r1, r2) =>
      if(r1 && r2) true
      else return false //as soon as false is encountered, break the loop and return false
    )
  }catch{case e: UnsupportedOperationException => true} //the folder is already empty => empty.reduceLeft throws the exception.
  /***** PUBLIC METHODS - START *****/
}


