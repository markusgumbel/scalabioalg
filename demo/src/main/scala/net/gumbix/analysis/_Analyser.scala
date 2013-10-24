package net.gumbix.analysis

import net.gumbix.analysis.DynPro._
import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.Stage._
import net.gumbix.analysis.Db._
import java.io.{File, PrintWriter}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 10/2/13
 * Time: 6:41 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
trait _Analyser {
  /********** PRIVATE ATTRIBUTES - START **********/
  protected val (statusCounter, results) = (Map[DynPro, Map[Long, Int]](), Map[DynPro, String]())
  private val ( //The storage block
    map, //The reference to the object storing all the results.
    seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds, //matrix
    seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds //total
  ) = (
    Map[Stage, GraphValues](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double](),
    Map[Long, Double](), Map[Long, Double](), Map[Long, Double](), Map[Long, Double]()
  )
  /********** PRIVATE ATTRIBUTES - END **********/


  /********** PRIVATE METHODS - START **********/
  /**
   * This method is used to save the values obtained during the analysis.
   * @param dp see DpSingleActor
   * @return true if both files have been successfully saved, false otherwise.
   */
  private def saveMap(dp: DynPro, fileName: String): Boolean = try{
    val writer1 = new PrintWriter(new File(fileName.format(dp, saveDate, MATRIX)))
    writer1.write(map(MATRIX).getText)
    writer1.close

    val writer2 = new PrintWriter(new File(fileName.format(dp, saveDate, TOTAL)))
    writer2.write(map(TOTAL).getText)
    writer2.close

    true
  }catch{case e: Exception => false}


  /**
   * This method is used to evaluate the values obtained
   * for a given -dyn pro alg and -sequence length
   * @param len see DpSingleActor
   * @param values
   */
  private def evaluateValues(
    nrOfCom: Int, len: Long,
    values: (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])
  ) = {
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
    seqMxMins += len -> sSeqMx.find(min => min != null).get //sSeqMx.head occasionally returns null
    conMxMins += len -> sConMx.find(min => min != null).get //sConMx.head occasionally returns null
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
  /********** PRIVATE METHODS - END **********/


  /********** PROTECTED METHODS - START **********/
  /**
   * This method is used to print the current status of the Analyser
   * @param dp see DpSingleActor
   * @param len see DpSingleActor
   * @param nr see DpSingleActor
   */
  protected def printStatus(dp: DynPro, len: Long, nr: Int, nrOfCom: Int) = {
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
   * This method is used to get the duration values of the sequential- and concurrent-
   * computations of the given dyn pro alg and sequence length.
   * @param dp see DpSingleActor
   * @param len see DpSingleActor
   * @return (Map[Stage, Double], Map[Stage, Double])
   *
   * @note This method isn't private because the DpActors need to access it.
   */
  def getTimeMap(dp: DynPro, len: Long): (Map[Stage, Double], Map[Stage, Double]) = dp match{
    case GLOBALG => DnaDynProRunner.runGlobalAlignment(len)
    case VITERBI => DnaDynProRunner.runViterbi(len)
  }


  /**
   * This method is used to save the storage
   * in which the values obtained during one round are stored before their evaluation.
   * @param storage see DpSingleActor
   */
  protected def saveStorage(
    lens: ListBuffer[Long], nrOfCom: Int, fileName: String,
    storage: Map[DynPro, Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]
  ) = storage.foreach{dpSto => //dynPro & len & values
    dpSto._2.foreach{lenSto => evaluateValues(nrOfCom, lenSto._1, lenSto._2)} //len & values

    //all the objects below have been adequately updated
    //during the invocations of the "evaluateValues" method above.
    map(MATRIX) = GraphValues(
      lens, seqMxMins, seqMxMaxs, seqMxMeds, seqMxAvgs, conMxMins, conMxMaxs, conMxMeds, conMxAvgs
    )
    map(TOTAL) = GraphValues(
      lens, seqTtMins, seqTtMaxs, seqTtMeds, seqTtAvgs, conTtMins, conTtMaxs, conTtMeds, conTtAvgs
    )

    results(dpSto._1) = if(saveMap(dpSto._1, fileName)) "SUCCESSFUL" else "ERROR"
  }
  /********** PROTECTED METHODS - END **********/
}
