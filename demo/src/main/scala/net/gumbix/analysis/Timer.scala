package net.gumbix.analysis

import scala.collection.mutable.{Map, ListBuffer}
import net.gumbix.dynpro.concurrency.Stage._
import DynPro._
import FactoringMode._
import java.text.SimpleDateFormat
import java.io.{PrintWriter, File}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 7:11 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[analysis] case class GraphValues(
  lens: ListBuffer[Long],
  seqMin: ListBuffer[Long],
  seqMax: ListBuffer[Long],
  seqMed: ListBuffer[Long],
  seqAvg: ListBuffer[Long],
  conMin: ListBuffer[Long],
  conMax: ListBuffer[Long],
  conMed: ListBuffer[Long],
  conAvg: ListBuffer[Long]
){
  def getText = {
    val (s, text) = (" ", new StringBuilder())
    for(i <- 0 until lens.length){
      if(i != 0) text.append("\n")
      text.append(lens(i)+s+seqMin(i)+s+seqMax(i)+s+seqMed(i)+s+seqAvg(i)+s+conMin(i)+s+conMax(i)+s+conMed(i)+s+conAvg(i))
    }
    text.toString
  }
}

/**
 *
 * @param nrOfCom number of computations per round
 */
protected[analysis] class Timer(nrOfCom: Int){
  /**
   *
   * @param minLen
   * @param maxLen
   * @param factor sequence length expansion factor
   * @param mode sequence length expansion mode
   * @param dynpro
   * @return
   */
  def runAnalysis(minLen: Int, maxLen: Long, factor: Int, mode: FactoringMode, dynpro: DynPro){
    var (curLen, round) = (if(minLen < 10) 10 else minLen, 0)
    val (map, curLens,
    seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds,
    seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds,
    med, med_1, nrOfSeqs) =
      (Map[Stage, GraphValues](), ListBuffer[Long](),
        ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](),
        ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](),
        ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](),
        ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](), ListBuffer[Long](),
        nrOfCom/2, nrOfCom/2 - 1, dynpro match{
          case GLOBALG => 2 * nrOfCom
          case VITERBI => nrOfCom
      })

    /**
     *
     */
    def printStatus{
      val prefix = "The analysis "
      val status = if(round == 0) prefix + "has started!"
        else if(curLen > maxLen) prefix + "has ended!"
        else prefix + "is running! Round: " + round

      println(status)
      round += 1
    }
    /**
     *
     * @return
     */
    def raiseCurLen = mode match{
      case ARI => curLen + factor
      case GEO => curLen * factor
      case EXP => math.pow(curLen, factor).asInstanceOf[Int]
    }
    /**
     *
     * @return
     */
    def saveMap: Boolean = {
      val core = Runtime.getRuntime().availableProcessors + "cores"
      val date = new SimpleDateFormat("yyyyMMddHHmmss").format(new java.util.Date())
      val s = "_"
      def filename(stage: Stage) = "matlab/results/"+dynpro+s+stage+s+core+s+date

      try{
        val writer1 = new PrintWriter(new File(filename(MATRIX)))
        writer1.write(map(MATRIX).getText)
        val writer2 = new PrintWriter(new File(filename(TOTAL)))
        writer2.write(map(TOTAL).getText)
        true
      }catch{case e: Exception => false}
    }


    printStatus
    while(curLen <= maxLen){
      printStatus

      val (seqs, seqMx, seqTt, conMx, conTt) =
        (DNASeqCreator.getSeqs(nrOfSeqs, curLen),
         new ListBuffer[Long](), new ListBuffer[Long](), new ListBuffer[Long](), new ListBuffer[Long]())

      dynpro match{
        case GLOBALG =>
          for(i <- 0 until nrOfCom){ //Debugger.printMemories
            val timeMap = DNASeqCreator.runGlobalAlignment(seqs(i), seqs(i+nrOfCom))
            seqMx += timeMap._1(MATRIX)
            seqTt += timeMap._1(TOTAL)
            conMx += timeMap._2(MATRIX)
            conTt += timeMap._2(TOTAL)
          }
        case VITERBI =>
          for(i <- 0 until nrOfCom){
            val timeMap = DNASeqCreator.runViterbi(seqs(i))
            seqMx += timeMap._1(MATRIX)
            seqTt += timeMap._1(TOTAL)
            conMx += timeMap._2(MATRIX)
            conTt += timeMap._2(TOTAL)
          }
      }

      val (sSeqMx, sSeqTt, sConMx, sConTt) = (seqMx.sorted, seqTt.sorted, conMx.sorted, conTt.sorted)

      val (seqMxMed, seqTtMed, conMxMed, conTtMed) = if(nrOfCom % 2 == 0)(
         (sSeqMx(med_1) + sSeqMx(med))/2,
         (sSeqTt(med_1) + sSeqTt(med))/2,
         (sConMx(med_1) + sConMx(med))/2,
         (sConTt(med_1) + sConTt(med))/2
      )else (sSeqMx(med), sSeqTt(med), sConMx(med), sConTt(med))

      //x-coordinate
      curLens += curLen
      //matrix
      seqMxMins += sSeqMx.head
      conMxMins += sConMx.head
      seqMxMaxs += sSeqMx.last
      conMxMaxs += sConMx.last
      seqMxAvgs += sSeqMx.reduceLeft(_ + _)/sSeqMx.length
      conMxAvgs += sConMx.reduceLeft(_ + _)/sConMx.length
      seqMxMeds += seqMxMed
      conMxMeds += conMxMed
      //total
      seqTtMins += sSeqTt.head
      conTtMins += sConTt.head
      seqTtMaxs += sSeqTt.last
      conTtMaxs += sConTt.last
      seqTtAvgs += sSeqTt.reduceLeft(_ + _)/sSeqTt.length
      conTtAvgs += sConTt.reduceLeft(_ + _)/sConTt.length
      seqTtMeds += seqTtMed
      conTtMeds += conTtMed

      curLen = raiseCurLen
    }
    printStatus

    map(MATRIX) = GraphValues(curLens, seqMxMins, seqMxMaxs, seqMxMeds, seqMxAvgs, conMxMins, conMxMaxs, conMxMeds, conMxAvgs)
    map(TOTAL) = GraphValues(curLens, seqTtMins, seqTtMaxs, seqTtMeds, seqTtAvgs, conTtMins, conTtMaxs, conTtMeds, conTtAvgs)
    saveMap
  }


}


