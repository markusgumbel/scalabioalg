package net.gumbix.analysis

import scala.collection.mutable.{Map, ListBuffer}
import net.gumbix.dynpro.concurrency.Stage._
import DynPro._
import FactoringMode._
import java.text.SimpleDateFormat
import java.util.Date
import java.io.{PrintWriter, File}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 7:11 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 *
 */
protected[analysis] object Timer{

  /**
   * The method raises the current sequence length.
   * @param mode see the "runAnalysis" method
   * @param curLen
   * @param factor see the "runAnalysis" method
   * @return
   */
  private def raiseCurLen(mode: FactoringMode, curLen: Long, factor: Int): Long = mode match{
    case ARI => curLen + factor
    case GEO => curLen * factor
    case EXP => math.pow(curLen, factor).asInstanceOf[Long]
  }

  private val dir = "demo~src~main~scala~net~gumbix~analysis~matlab~results~"
    .replace("~", File.separator) //to avoid a operating system dependency
  private val (_fileName, prefix) = (dir + "%s_%s_%scores_%s", "The analysis ")

  private val (saveFormat, anaFormat) = (
    new SimpleDateFormat("yyyyMMddHHmmss"), new SimpleDateFormat("dd.MM.yyyy - HH:mm:ss")
  )
  private def saveDate = saveFormat.format(new Date())
  private def anaDate = anaFormat.format(new Date())

  val cores = Runtime.getRuntime.availableProcessors
  /**
   * This method is used to save the values obtained during the analysis.
   * @param dynpro see the "runAnalysis" method
   * @param map
   * @return true if both files have been successfully saved, false otherwise.
   */
  private def saveMap(dynpro: DynPro, map: Map[Stage, GraphValues]): Boolean = {
    def filename(stage: Stage) = _fileName.format(dynpro, stage, cores, saveDate)

    try{
      val writer1 = new PrintWriter(new File(filename(MATRIX)))
      writer1.write(map(MATRIX).getText)
      writer1.close

      val writer2 = new PrintWriter(new File(filename(TOTAL)))
      writer2.write(map(TOTAL).getText)
      writer2.close

      true
    }catch{case e: Exception => false}
  }


  /**
   *
   * @param minLen
   * @param maxLen
   * @param nrOfCom number of computations per round
   * @param factor sequence length expansion factor
   * @param mode sequence length expansion mode
   * @param dynpro The dynamic programming algorithm used to run the analysis.
   * @return true if the analysis is successful and all results have been successfully saved, false otherwise.
   */
  private def runAnalysis(minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, mode: FactoringMode, dynpro: DynPro) = {
    if(nrOfCom > 0 && minLen >= 100 && maxLen > minLen && factor > 0){
      var (curLen: Long, round) = (minLen.asInstanceOf[Long], 0)
      val (
        map, curLens,
        seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds,
        seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds,
        med, med_1, nrOfSeqs
      ) = (
        Map[Stage, GraphValues](), ListBuffer[Double](),
        ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](),
        ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](),
        ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](),
        ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](), ListBuffer[Double](),
        nrOfCom/2, nrOfCom/2 - 1, dynpro match{
          case GLOBALG => 2 * nrOfCom
          case VITERBI => nrOfCom
        }
      )

      /**
       *
       */
      def printStatus{
        val status = if(round == 0) prefix + "of \"%s\" has started! [%s]".format(dynpro, anaDate)
          else if(curLen > maxLen) prefix + "of \"%s\" has ended! [%s]".format(dynpro, anaDate)
          else "\t" + prefix + "is running! Round: %s - Sequence length: %s [%s]".format(round, curLen, anaDate)

        println(status)
        round += 1
      }


      /***** LOOP - START *****/
      var (lastRound, keepWhileLoopAlive) = (false, true)
      printStatus
      while(keepWhileLoopAlive){ //curLen <= maxLen
        printStatus

        val (seqs, seqMx, seqTt, conMx, conTt) =
          (DNASeqCreator.getSeqs(nrOfSeqs, curLen), //the sequences
           new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double]())

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
              conMx += timeMap._2(MATRIX) //this isn't an error. The editor is just acting up.
              conTt += timeMap._2(TOTAL) //this isn't an error. The editor is just acting up.
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

        //matrix (minimum, maximum, average, median)
        seqMxMins += sSeqMx.head
        conMxMins += sConMx.head
        seqMxMaxs += sSeqMx.last
        conMxMaxs += sConMx.last
        seqMxAvgs += sSeqMx.reduceLeft(_ + _)/sSeqMx.length
        conMxAvgs += sConMx.reduceLeft(_ + _)/sConMx.length
        seqMxMeds += seqMxMed
        conMxMeds += conMxMed

        //total (minimum, maximum, average, median)
        seqTtMins += sSeqTt.head
        conTtMins += sConTt.head
        seqTtMaxs += sSeqTt.last
        conTtMaxs += sConTt.last
        seqTtAvgs += sSeqTt.reduceLeft(_ + _)/sSeqTt.length
        conTtAvgs += sConTt.reduceLeft(_ + _)/sConTt.length
        seqTtMeds += seqTtMed
        conTtMeds += conTtMed

        //curLen = raiseCurLen(mode, curLen, factor)

        if(lastRound) keepWhileLoopAlive = false
        else{
          curLen = raiseCurLen(mode, curLen, factor)
          if(curLen >= maxLen){
            curLen = maxLen
            lastRound = true
          }
        }

      }
      printStatus
      /***** LOOP - END *****/

      map(MATRIX) = GraphValues(curLens, seqMxMins, seqMxMaxs, seqMxMeds, seqMxAvgs, conMxMins, conMxMaxs, conMxMeds, conMxAvgs)
      map(TOTAL) = GraphValues(curLens, seqTtMins, seqTtMaxs, seqTtMeds, seqTtAvgs, conTtMins, conTtMaxs, conTtMeds, conTtAvgs)

      saveMap(dynpro, map)
    }else false
  }


  /**
   *
   * @param minLen
   * @param maxLen
   * @param nrOfCom
   * @param factor
   * @param mode
   * @param dynpro
   * @return
   */
  def runAnalysis(minLen: Int, maxLen: Long, nrOfCom: Int, factor: Int, mode: FactoringMode, dynpro: DynPro*): Seq[Boolean] = {
    println("START...[%s]".format(anaDate))
    val res = dynpro.map(dp => runAnalysis(minLen, maxLen, nrOfCom, factor, mode, dp))
    println("END...[%s]\nResults: %s".format(anaDate, res.mkString(", ")))

    res
  }


  /**
   * This method is used to the clear the result directory.
   * @return true if all files in the result directory have been successfully deleted
   *         otherwise false.
   */
  def cleanResultDir: Boolean = {
    new File(dir).listFiles.map(f => f.delete).reduceLeft((r1, r2) => if(r1 && r2) true else false)
  }
}


