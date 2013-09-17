package net.gumbix.analysis

import scala.actors.Actor
import scala.collection.mutable.{ListBuffer, Map}

import java.io.{PrintWriter, File}

import net.gumbix.analysis.FactoringMode._
import net.gumbix.analysis.DynPro._
import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Stage._
import net.gumbix.dynpro.concurrency.Messages._


/**
 * An algorithm for data transfer.
 * Project name: scabio
 * Date: 9/15/13
 * Time: 11:22 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This actor concurrently runs the analysis of ALL the given dyn pro alg's.
 * @see The "run" method below to find out the purpose of each parameter.
 */
private class AnalyserActor(
  minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, mode: FactoringMode, dynPros: Seq[DynPro]
) extends Actor{
  /********** PRIVATE ATTRIBUTES - START **********/
  private val ( //The storage block
    _fileName, statusCounter, results, map, lens,
    seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds, //matrix
    seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds //total
    ) = (
    dir + "%s_%s_" + "core=%s_min=%s_max=%s_nrofcomp=%s_factor=%s_mode=%s_".format(
      cores, minLen, maxLen, nrOfCom, factor, mode
    ) + "%s.scabio",
    Map[DynPro, Map[Long, Int]](),
    Map[DynPro, String](), Map[Stage, GraphValues](), getLens,
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double]()
  )
  /********** PRIVATE ATTRIBUTES - END **********/


  /********** PRIVATE METHODS - START **********/
  /**
   * This method is used to print the current status of the Analyser
   * @param dp see DpActor
   * @param len see DpActor
   * @param nr see DpActor
   */
  private def printStatus(dp: DynPro, len: Long, nr: Int) = {
    statusCounter(dp)(len) += 1 //update
    val (_nr, date) = (statusCounter(dp)(len), anaDate)

    val status =
      (if(_nr == 1) "[%s] => {%s | %s} =: START!\n".format(date, dp, len) else "") +
        //this is the 3rd of 5 milestones
      "\t[%s] => {%s | %s | #%s (%s/%s)} =: DONE!".format(date, dp, len, nr, _nr, nrOfCom) +
      (if(_nr == nrOfCom) "\n[%s] => {%s | %s} =: DONE!".format(date, dp, len) else "")
        //this is the 4th of 5 milestones

    println(status)
  }

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
   * This method is used to save the values obtained during the analysis.
   * @param dp see DpActor
   * @param map The reference to the object storing all the results.
   * @return true if both files have been successfully saved, false otherwise.
   */
  private def saveMap(dp: DynPro, map: Map[Stage, GraphValues]): Boolean = try{
    //return the adequate file name
    def filename(stage: Stage) = _fileName.format(dp, stage, saveDate)

    val writer1 = new PrintWriter(new File(filename(MATRIX)))
    writer1.write(map(MATRIX).getText)
    writer1.close

    val writer2 = new PrintWriter(new File(filename(TOTAL)))
    writer2.write(map(TOTAL).getText)
    writer2.close

    true
  }catch{case e: Exception => false}

  /**
   * This method is used to get the set of all sequence lengths to be computed.
   * @return ListBuffer[Long](100, 150,..., 20000)
   */
  private def getLens = {
    val lens = ListBuffer[Long]()
    var (lastRound, keepWhileLoopAlive, len) = (false, true, minLen.asInstanceOf[Long])
    while(keepWhileLoopAlive){
      lens += len //x-coordinates
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
    lens
  }

  /**
   * This method is used to get the duration values of the sequential- and concurrent-
   * computations of the given dyn pro alg and sequence length.
   * @param dp see DpActor
   * @param len see DpActor
   * @return (Map[Stage, Double], Map[Stage, Double])
   */
  private def getTimeMap(dp: DynPro, len: Long) = dp match{
    case GLOBALG => DnaDynProRunner.runGlobalAlignment(len)
    case VITERBI => DnaDynProRunner.runViterbi(len)
  }

  /**
   * This method is used to evaluate the values obtained
   * for a given -dyn pro alg and -sequence length
   * @param len see DpActor
   * @param values
   */
  private def evaluateValues(len: Long, values:
    (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])
  ) = {
    /* values =:
     *   seqMx: ListBuffer[Double], seqTt: ListBuffer[Double], conMx: ListBuffer[Double], conTt: ListBuffer[Double]*/

    //collect the required values
    val (sSeqMx, sSeqTt, sConMx, sConTt, med, _med) = (
      values._1.sorted, values._2.sorted, values._3.sorted, values._4.sorted, nrOfCom/2, nrOfCom/2 - 1
    )
    val (seqMxMed, seqTtMed, conMxMed, conTtMed) = if(nrOfCom % 2 == 0)(
      (sSeqMx(_med) + sSeqMx(med))/2,
      (sSeqTt(_med) + sSeqTt(med))/2,
      (sConMx(_med) + sConMx(med))/2,
      (sConTt(_med) + sConTt(med))/2
    )else (sSeqMx(med), sSeqTt(med), sConMx(med), sConTt(med))

    //matrix (minimum, maximum, average, median)
    seqMxMins += len -> sSeqMx.head
    conMxMins += len -> sConMx.head
    seqMxMaxs += len -> sSeqMx.last
    conMxMaxs += len -> sConMx.last
    seqMxAvgs += len -> sSeqMx.reduceLeft(_ + _)/sSeqMx.length
    conMxAvgs += len -> sConMx.reduceLeft(_ + _)/sConMx.length
    seqMxMeds += len -> seqMxMed
    conMxMeds += len -> conMxMed

    //total (minimum, maximum, average, median)
    seqTtMins(len) = sSeqTt.head
    conTtMins(len) = sConTt.head
    seqTtMaxs(len) = sSeqTt.last
    conTtMaxs(len) = sConTt.last
    seqTtAvgs(len) = sSeqTt.reduceLeft(_ + _)/sSeqTt.length
    conTtAvgs(len) = sConTt.reduceLeft(_ + _)/sConTt.length
    seqTtMeds(len) = seqTtMed
    conTtMeds(len) = conTtMed
  }

  /**
   * This method is used to save the storage
   * in which the values obtained during one round are stored before their evaluation.
   * @param storage see DpActor
   */
  private def saveStorage(storage: Map[DynPro, Map[Long,
    (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])
  ]]) = storage.foreach{dpSto => //dynPro & len & values
    dpSto._2.foreach{lenSto => evaluateValues(lenSto._1, lenSto._2)} //len & values

    //all the objects below have been adequately updated
    //during the invocations of the "evaluateValues" method above.
    map(MATRIX) = GraphValues(
      lens,
      seqMxMins, seqMxMaxs, seqMxMeds, seqMxAvgs, conMxMins, conMxMaxs, conMxMeds, conMxAvgs
    )
    map(TOTAL) = GraphValues(
      lens,
      seqTtMins, seqTtMaxs, seqTtMeds, seqTtAvgs, conTtMins, conTtMaxs, conTtMeds, conTtAvgs
    )

    results(dpSto._1) = if(saveMap(dpSto._1, map)) "SUCCESSFUL" else "ERROR"
  }
  /********** PRIVATE METHODS - END **********/


  /********** ACT METHOD - START **********/
  def act{react{case START =>
    if(minLen >= 100 && maxLen > minLen && nrOfCom > 0 && factor > 0
      && mode.isInstanceOf[FactoringMode] && dynPros.isInstanceOf[Seq[DynPro]]){
      val (to, storage) = ( //the storage in which the values obtained during one round are stored before their evaluation.
        sender,
        Map[DynPro, Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]()
        //the storage dp -> len -> (current) nrOfCom -> (seqMx, seqTt, conMx, conTt)
      )
      lazy val andThenBlock = { //used once the loopWhile is done
        saveStorage(storage)
        to ! results
      }

      dynPros.foreach{dp => if(dp.isInstanceOf[DynPro]){ //dyn pro alg iteration
        //initializing the objects
        storage(dp) = Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]()
        statusCounter(dp) = Map[Long, Int]()

        //running the computations
        lens.foreach{len => //sequence length iteration
          //initializing the objects
          storage(dp) += len ->
            (new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double]())
          statusCounter(dp) += len -> 0

          //running the computations
          (0 until nrOfCom).foreach{nr => //number of computation iteration
            new DpActor(this, dp, len, nr, storage(dp)(len)){ //extension of the DpActor
              lazy val timeMap = getTimeMap(dp, len)
            }.start
          }
        }
      }else results(dp) = "ERROR"}

      println("[%s] All the DpActors have been started. WAITING for the results!".format(anaDate))
        //This is the 3rd of 5 milestones
      var loopStart = storage.size * lens.length * nrOfCom //number of all the actors created
      loopWhile(loopStart > 0){react{case (dp: DynPro, len: Long, nr: Int) =>
        printStatus(dp, len, nr)
        loopStart -= 1
      }}andThen andThenBlock
    }else{
      dynPros.foreach(dp => results(dp) = "ERROR")
      reply(results)
    }
  }}
  /********** ACT METHOD - END **********/
}


/**
 * This actor is used (by extension) to concurrently run the sequential- and concurrent modes of a chosen dyn pro alg.
 * @param actor The master actor.
 * @param dp dyn pro alg
 * @param len A sequence length.
 * @param nr A number of computation.
 * @param storage The storage in which the values obtained during one round are stored before their evaluation.
 */
private abstract class DpActor(
  actor: AnalyserActor, dp: DynPro, len: Long, nr: Int,
  storage:(ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])
) extends Actor{
  val timeMap: (Map[Stage, Double], Map[Stage, Double])
  //The results of the method invoked to run the sequential- and concurrent modes of a chosen dyn. pro. algorithm.

  def act{
    storage._1 += timeMap._1(MATRIX) //seqMx
    storage._2 += timeMap._1(TOTAL) //seqTt
    storage._3 += timeMap._2(MATRIX) //conMx
    storage._4 += timeMap._2(TOTAL) //conTt
    actor ! (dp, len, nr+1)
  }
}
