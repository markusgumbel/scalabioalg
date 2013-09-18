package net.gumbix.analysis

import scala.actors.{Actor, TIMEOUT}
import scala.collection.mutable.{ListBuffer, Map}

import java.io.{PrintWriter, File}

import net.gumbix.analysis.FactoringMode._
import net.gumbix.analysis.DynPro._
import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Stage._
import net.gumbix.dynpro.concurrency.Messages._
import net.gumbix.dynpro.concurrency.Debugger


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
  minLen: Int, maxLen: Long, lens: (ListBuffer[Long], ListBuffer[Long], ListBuffer[Long]),
  nrOfCom: Int, factor: Int, mode: FactoringMode, dynPros: Seq[DynPro]
) extends Actor{
  /********** PRIVATE ATTRIBUTES - START **********/
  private val ( //The storage block
    _fileName, statusCounter, results, map,
    seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds, //matrix
    seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds //total
    ) = (
    dir + "%s_%s_" + "core=%s_min=%s_max=%s_nrofcomp=%s_factor=%s_mode=%s_".format(
      cores, minLen, maxLen, nrOfCom, factor, mode
    ) + "%s.scabio",
    Map[DynPro, Map[Long, Int]](),
    Map[DynPro, String](), Map[Stage, GraphValues](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double]()
  )
  /********** PRIVATE ATTRIBUTES - END **********/


  /********** PRIVATE METHODS - START **********/
  /**
   * This method is used to print the current status of the Analyser
   * @param dp see DpSingleActor
   * @param len see DpSingleActor
   * @param nr see DpSingleActor
   */
  private def printStatus(dp: DynPro, len: Long, nr: Int) = {
    statusCounter(dp)(len) += 1 //update
    val (_nr, date) = (statusCounter(dp)(len), anaDate)

    val status =
      (if(_nr == 1) "%s{%s | %s} =: START!\n".format(date, dp, len) else "") +
        //this is the 3rd of 5 milestones
      "\t%s{%s | %s | #%s (%s/%s)} =: DONE!".format(date, dp, len, nr, _nr, nrOfCom) +
      (if(_nr == nrOfCom) "\n%s{%s | %s} =: DONE!".format(date, dp, len) else "")
        //this is the 4th of 5 milestones

    println(status)
  }

  /**
   * This method is used to save the values obtained during the analysis.
   * @param dp see DpSingleActor
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
   * This method is used to get the duration values of the sequential- and concurrent-
   * computations of the given dyn pro alg and sequence length.
   * @param dp see DpSingleActor
   * @param len see DpSingleActor
   * @return (Map[Stage, Double], Map[Stage, Double])
   *
   * @note This method isn't private because the DpActors need to access it.
   */
  def getTimeMap(dp: DynPro, len: Long) = dp match{
    case GLOBALG => DnaDynProRunner.runGlobalAlignment(len)
    case VITERBI => DnaDynProRunner.runViterbi(len)
  }

  /**
   * This method is used to evaluate the values obtained
   * for a given -dyn pro alg and -sequence length
   * @param len see DpSingleActor
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
   * @param storage see DpSingleActor
   */
  private def saveStorage(storage: Map[DynPro,
    Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]
  ) = storage.foreach{dpSto => //dynPro & len & values
    dpSto._2.foreach{lenSto => evaluateValues(lenSto._1, lenSto._2)} //len & values

    //all the objects below have been adequately updated
    //during the invocations of the "evaluateValues" method above.
    map(MATRIX) = GraphValues(
      lens._1,
      seqMxMins, seqMxMaxs, seqMxMeds, seqMxAvgs, conMxMins, conMxMaxs, conMxMeds, conMxAvgs
    )
    map(TOTAL) = GraphValues(
      lens._1,
      seqTtMins, seqTtMaxs, seqTtMeds, seqTtAvgs, conTtMins, conTtMaxs, conTtMeds, conTtAvgs
    )

    results(dpSto._1) = if(saveMap(dpSto._1, map)) "SUCCESSFUL" else "ERROR"
  }
  /********** PRIVATE METHODS - END **********/


  /********** ACT METHOD - START **********/
  def act{react{case START =>
    val (to, storage) = ( //the storage in which the values obtained during one round are stored before their evaluation.
      sender,
      Map[DynPro, Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]()
      //the storage dp -> len -> (current) nrOfCom -> (seqMx, seqTt, conMx, conTt)
    )
    lazy val andThenBlock = { //used once the loopWhile is done
      saveStorage(storage) //the results object is updated in this method
      to ! results
    }

    def iniStorage(dp: DynPro, len: Long){
      //initializing the objects
      storage(dp) += len ->
        (new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double]())
      statusCounter(dp) += len -> 0
    }

    dynPros.foreach{dp => if(dp.isInstanceOf[DynPro]){ //dyn pro alg iteration
      //initializing the objects
      storage(dp) = Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]()
      statusCounter(dp) = Map[Long, Int]()

      /* LEFT SIDE OF THE BORDER => the running the computations by using the DpActor in "multi" mode:
       * - less actors created (good for the GC),
       * - each of them computing more than one of the shortest sequences
       *   (balancing this way the load among the DpActors).
       */
      lens._2.foreach{len => iniStorage(dp, len)}
      (0 until nrOfCom).foreach{nr => //number of computed iteration
        new DpActor(this, dp, lens._2, nr, storage(dp)).start
      }

      /* RIGHT SIDE OF THE BORDER => running the computations by using the DpActor in "single" mode:
       * - one DpActor for each sequence length and computation number (less good for the GC),
       * - however boast the Analyser speed.
       */
      lens._3.foreach{len => //sequence length iteration
        iniStorage(dp, len)
        (0 until nrOfCom).foreach{nr => //running the computations
          new DpActor(this, dp, ListBuffer(len), nr, storage(dp)).start
        }
      }
    }else results(dp) = "ERROR"}

    println(anaDate + "All the DpActors have been started. The Analyser is WAITING for the results!")
      //This is the 3rd of 5 milestones
    var loopStart = storage.size * lens._1.length * nrOfCom //number of all the actors created
    loopWhile(loopStart > 0){reactWithin(300000){ //react within 5 min
      case (dp: DynPro, len: Long, nr: Int) =>
        printStatus(dp, len, nr)
        loopStart -= 1
      case TIMEOUT => println(anaDate + "The Analyser is still WAITING for the results!")
        //This is the 3rd of 5 milestones
    }}andThen andThenBlock
  }}
  /********** ACT METHOD - END **********/
}


/**
 * This actor is used (by extension) to concurrently run the sequential- and concurrent modes of a chosen dyn pro alg.
 * @param actor The master actor.
 * @param dp dyn pro alg
 * @param lens The set of sequence length.
 * @param nr A number of computation.
 * @param storage The storage in which the values obtained during one round are stored before their evaluation.
 */
private final class DpActor(
  actor: AnalyserActor, dp: DynPro, lens: ListBuffer[Long], nr: Int,
  storage: Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]
) extends Actor{
  private def store(
    storage:(ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double]),
    timeMap: (Map[Stage, Double], Map[Stage, Double]), len: Long
  ){
    storage._1 += timeMap._1(MATRIX) //seqMx
    storage._2 += timeMap._1(TOTAL) //seqTt
    storage._3 += timeMap._2(MATRIX) //conMx
    storage._4 += timeMap._2(TOTAL) //conTt
    actor ! (dp, len, nr+1)
  }

  def act{lens.foreach{len =>
    store(storage(len), actor.getTimeMap(dp, len), len)
  }}
}
