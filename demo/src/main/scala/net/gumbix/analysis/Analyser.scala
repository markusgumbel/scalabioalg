package net.gumbix.analysis

import scala.collection.mutable.Map

import java.io.File

import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Messages
import Messages._
import DynPro._
import FactoringMode._

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
 *   by creating a "DpActor" for each of them.
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
 *@note For test purposes i have raised my
 *       - JAVA Maximum heap size to Xmx4096m and
 *       - IntelliJ Maximum heap size to Xmx1024m
 *       That's as far as i can go.
 */
protected[analysis] object Analyser{
  /**
   * The method is invoked to start the analysis.
   * @param minLen The minimum length
   * @param maxLen The maximum length
   * @param nrOfCom The number of computations per round
   * @param factor The sequence length expansion factor
   * @param mode The sequence length expansion mode
   * @param dynpro The dyn pro alg used to run the analysis.
   * @return True (SUCCESSFUL) if the analysis is successful and all results have been successfully saved,
   *         false (ERROR) otherwise.
   */
  def run(minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, mode: FactoringMode, dynpro: DynPro*): Map[DynPro, String] = {
    println("[%s] START...".format(anaDate))

    val actor = new AnalyserActor(minLen, maxLen, nrOfCom, factor, mode, dynpro)
    actor.start
    val results = actor !? START match{case results: Map[DynPro, String] => results}

    println("[%s] END...\nResults: %s".format(anaDate, results.mkString(", ")))
    results
  }


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
  }catch{case e: UnsupportedOperationException => true} //the folder is already empty
  /***** PUBLIC METHODS - START *****/
}


