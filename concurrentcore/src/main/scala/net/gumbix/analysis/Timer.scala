package net.gumbix.analysis

import scala.collection.mutable.{Map, ListBuffer}
import net.gumbix.dynpro.concurrency.Stage._
import DynPro._
import FactoringMode._
import AnaVal._
import scala.collection.mutable
import net.gumbix.dynpro.concurrency.Debugger

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 7:11 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

protected[analysis] case class GraphValues(lens: ListBuffer[Long], seqs: ListBuffer[Long], cons: ListBuffer[Long])

/**
 *
 * @param _minLen
 * @param maxLen
 * @param factor
 * @param mode
 * @param nrOfCom number of computations per round
 */
protected[analysis] class Timer(_minLen: Int, maxLen: Long, factor: Int, mode: FactoringMode, nrOfCom: Int){

  private val minLen = if(_minLen < 10) 10 else _minLen

  private def raiseCurLen(curLen: Int) = mode match{
    case ARI => curLen + factor
    case GEO => curLen * factor
    case EXP => math.pow(curLen, factor).asInstanceOf[Int]
  }


  def runAnalysis(dynpro: DynPro): Map[Stage, Map[AnaVal, GraphValues]] = {
    var (curLen, round) = (minLen, 0)
    val (map, curLens,
    seqMxMins, conMxMins, seqMxMaxs, conMxMaxs, seqMxAvgs, conMxAvgs, seqMxMeds, conMxMeds,
    seqTtMins, conTtMins, seqTtMaxs, conTtMaxs, seqTtAvgs, conTtAvgs, seqTtMeds, conTtMeds,
    med, med_1, nrOfSeqs) =
      (Map[Stage, Map[AnaVal, GraphValues]](), ListBuffer[Long](),
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
            seqMx += timeMap._1(matrix)
            seqTt += timeMap._1(total)
            conMx += timeMap._2(matrix)
            conTt += timeMap._2(total)
          }
        case VITERBI =>
          for(i <- 0 until nrOfCom){
            val timeMap = DNASeqCreator.runViterbi(seqs(i))
            seqMx += timeMap._1(matrix)
            seqTt += timeMap._1(total)
            conMx += timeMap._2(matrix)
            conTt += timeMap._2(total)
          }
      }

      val (sSeqMx, sSeqTt, sConMx, sConTt) = (seqMx.sorted, seqTt.sorted, conMx.sorted, conTt.sorted)

      val (seqMxMed, seqTtMed, conMxMed, conTtMed) = if(nrOfCom % 2 == 0)
        ((sSeqMx(med_1) + sSeqMx(med))/2,
         (sSeqTt(med_1) + sSeqTt(med))/2,
         (sConMx(med_1) + sConMx(med))/2,
         (sConTt(med_1) + sConTt(med))/2)
      else (sSeqMx(med), sSeqTt(med), sConMx(med), sConTt(med))

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

      curLen = raiseCurLen(curLen)
    }
    printStatus

    map(matrix)(MIN) = GraphValues(curLens, seqMxMins, conMxMins)
    map(matrix)(MAX) = GraphValues(curLens, seqMxMaxs, conMxMaxs)
    map(matrix)(AVG) = GraphValues(curLens, seqMxAvgs, conMxAvgs)
    map(matrix)(MED) = GraphValues(curLens, seqMxMeds, conMxMeds)
    map(total)(MIN) = GraphValues(curLens, seqTtMins, conTtMins)
    map(total)(MAX) = GraphValues(curLens, seqTtMaxs, conTtMaxs)
    map(total)(AVG) = GraphValues(curLens, seqTtAvgs, conTtAvgs)
    map(total)(MED) = GraphValues(curLens, seqTtMeds, conTtMeds)

    map
  }

}
