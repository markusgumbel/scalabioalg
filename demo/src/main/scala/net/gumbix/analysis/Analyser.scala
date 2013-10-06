package net.gumbix.analysis

import collection.mutable.{ListBuffer, Map}

import java.io.File

import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Messages
import Messages._
import DynPro._
import FactoringMode._
import AnalyserMode._
import scala.actors.Actor

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
 *  This term is used to defined the time required for the "Analyser" object (actually the "conRun" method)
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
 *      It is conventionally named after the original DynPro class with the prefix "conRun".
 * 3- Within the "match" block of the net.gumbix.analysis.AnalyserActor.getTimeMap method,
 *    create case handling the identifier created in step 1 by simply invoking the method created in step 2.
 * 4- One can now include the new identifier in the net.gumbix.analysis.demo.AnalyserApp and conRun it.
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
   * @param mode see the "conRun" method
   * @param len The current sequence length
   * @param factor see the "conRun" method
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
  private def getLens(leftLen: Int, rightLen: Long, switch: Double, mode: FactoringMode, factor: Int) = {
    val (minToMax, lens, lensLeft, lensRight) = (
      leftLen < rightLen, //ascending or descending
      ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long]()
    )
    var len: Long = 0
    val maxLen: Long = if(minToMax){len = leftLen; rightLen}else{len = rightLen; leftLen}
    var (lastRound, keepWhileLoopAlive) = (false, true)

    while(keepWhileLoopAlive){//the loop always raises the len value
      lens += len //x-coordinates
      //println(switch)
      if(len < switch) lensLeft += len else lensRight += len //x-coordinates used by the DpActors

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

    if(minToMax) (lens, lensLeft, lensRight) //leftLen < rightLen
    else (lens.reverse, lensRight.reverse, lensLeft.reverse) //leftLen > rightLen
  }

  /**
   * The method is invoked to start the analysis.
   * @param leftLen The left limit length
   * @param rightLen The right limit length
   * @param nrOfCom The number of computations per round
   * @param factor The sequence length expansion factor
   * @param switch The switch from left to right determining
   *               where to start to compute dyn pro alg's concurrently.
   * @param fMode The sequence length expansion mode
   * @param aMode The analyser running mode
   * @param dynPros The dyn pro alg's used to conRun the analysis.
   * @return True (SUCCESSFUL) if the analysis is successful and all results have been successfully saved,
   *         false (ERROR) otherwise.
   */
  private def _run(
    leftLen: Int, rightLen: Long, nrOfCom: Int, factor: Int, switch: Double,
    fMode: FactoringMode, aMode: AnalyserMode,
    block:(AnalyserMode, (ListBuffer[Long], ListBuffer[Long], ListBuffer[Long]), Int, Seq[DynPro], String) => Map[DynPro, String],
    dynPros: Seq[DynPro]
  ) = if(leftLen >= 100 && rightLen >= 100 && nrOfCom >= 10 && factor >= 0
    && fMode.isInstanceOf[FactoringMode] && aMode.isInstanceOf[AnalyserMode] && dynPros.isInstanceOf[Seq[DynPro]]){

    val lens = if(leftLen == rightLen)
      (ListBuffer[Long](leftLen), ListBuffer[Long](), ListBuffer[Long](leftLen))
    else if(switch >= -1 && switch <= 2) //-> ratio switch
      getLens(leftLen, rightLen, leftLen + (rightLen - leftLen) * switch, fMode, factor)
    else getLens(leftLen, rightLen, switch, fMode, factor) //-> value switch

    val options = "{range = %s ... %s | nrOfCom = %s | factor = %s | factoring mode = %s | nrOfActors = %s |%s DynPro(s) = %s} =: "
    .format(
      leftLen, rightLen, nrOfCom, factor,
      fMode, 1 + 2*nrOfCom * (lens._3.length + 1),
      //nrOfActors =: Analyser + (nrOfCom {for lensLeft} + nrOfCom*lensRight) *2 {sequential, concurrent}
      if(dynPros.length > 1) " analyser mode = %s | " else "",
      dynPros.mkString(", ")
    )
    val fileName = dir + "%s_%s_%s_" + "core=%s_leftLen=%s_rightLen=%s_nrofcomp=%s_factor=%s_mode=%s.scabio".format(
      cores, leftLen, rightLen, nrOfCom, factor, fMode
    )
    //Runtime.getRuntime.exec("Taskmgr.exe")//open the task-manager
    //unfortunately this throws the following exception
    //java.io.IOException: Cannot conRun program "Taskmgr.exe": CreateProcess error=740, Der angeforderte Vorgang erfordert erhöhte Rechte

    println(anaDate + options + "START!") //this is the 1st of 5 milestones
    val results = block(aMode, lens, nrOfCom, dynPros, fileName)
    println(anaDate + options + "END!\nResults: " + results.mkString(", "))
    //this is the 5th and last milestone.

    results
  }else{
    val (prefix, report) = ("\n\t", new StringBuilder(anaDate + "The Analyser couldn't start."))
    def updateReport(text: String){report ++= prefix + text + "."}

    if(math.min(leftLen, rightLen) < 100) updateReport("The minimum sequence length is 100")
    if(nrOfCom < 10) updateReport("The minimum number of computation per round is 10")
    if(factor <= 0) updateReport("The factor must be a positive number different than 0")
    if(!fMode.isInstanceOf[FactoringMode]) updateReport("The factoring mode is of type FactoringMode")
    if(!aMode.isInstanceOf[AnalyserMode]) updateReport("The analyser mode is of type AnalyserMode")
    if(!dynPros.isInstanceOf[Seq[DynPro]]) updateReport("The dynPros object is of type Seq[DynPro]")

    System.err.println(report)
    null
  }


  /**
   * The concurrent wrapper of the "_run" method.
   * @see _run for more information.
   */
  def conRun(
    leftLen: Int, rightLen: Long, nrOfCom: Int, factor: Int, switch: Double,
    fMode: FactoringMode, aMode: AnalyserMode, dynPros: DynPro*
  ) = _run(leftLen, rightLen, nrOfCom, factor, switch, fMode, aMode,
    (aMode, lens, nrOfCom, dynPros, fileName) => aMode match{
      case CON =>
        val actor = new AnalyserActor(lens, nrOfCom, dynPros, fileName)
        actor.start
        actor !? START match{case results: Map[DynPro, String] => results}

      case SEQ =>
        val results = Map[DynPro, String]()
        dynPros.map{dp =>
          val actor = new AnalyserActor(lens, nrOfCom, Seq(dp), fileName)
          actor.start
          actor !? START match{case _results: Map[DynPro, String] =>
            val key = _results.keySet.head
            results(key) = _results(key)
          }
        }
        results
    },
    dynPros
  )


  /**
   * The sequential wrapper of the "_run" method.
   * @see _run for more information.
   */
  def seqRun(
    leftLen: Int, rightLen: Long, nrOfCom: Int, factor: Int,
    fMode: FactoringMode, aMode: AnalyserMode, dynPros: DynPro*
  ) = _run(leftLen, rightLen, nrOfCom, factor, 1.1, fMode, aMode,
    (_, lens, nrOfCom, dynPros, fileName) => aMode match{
      case CON =>
        val actor = new Actor{
          private val (_this, results, maxLen) = (this, Map[DynPro, String](), dynPros.length)
          private class _Actor(dp: DynPro) extends Actor{
            def act{
              _this ! new AnalyserNonActor(lens._1, nrOfCom, Seq(dp), fileName).start
            }
          }

          def act{react{case START =>
            val to = sender
            dynPros.foreach{dp => new _Actor(dp).start}

            loopWhile(results.size < maxLen){react{
              case _results: Map[DynPro, String] =>
                val key = _results.keySet.head
                results(key) = _results(key)
            }}andThen to ! results
          }}
        }
        actor.start
        actor !? START match{case results: Map[DynPro, String] => results}

      case SEQ => new AnalyserNonActor(lens._1, nrOfCom, dynPros, fileName).start
    },
    dynPros
  )


  /**
   * This method is used to the clear the result directory.
   * @return true if all files in the result directory have been successfully deleted
   *         otherwise false.
   */
  def cleanResultDir: Boolean = {
    val files = new File(dir).listFiles
    if(files.nonEmpty){
      files.map(f => f.delete).reduceLeft((r1, r2) =>
        if(r1 && r2) true
        else return false //as soon as false is encountered, break the loop and return false
      )
    }else true //the folder is already empty => empty.reduceLeft throws the exception.
  }

  /**
   * Use this method with caution
   * @return
   */
  def startMatLab = try{
    println(anaDate + "Starting MatLab...")
    Runtime.getRuntime.exec("MATLAB.exe")
  }catch{case e: Exception => println(e.getMessage)}
  /***** PUBLIC METHODS - START *****/
}


