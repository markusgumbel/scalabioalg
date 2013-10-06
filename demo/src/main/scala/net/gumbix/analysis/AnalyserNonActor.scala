package net.gumbix.analysis

import scala.collection.mutable.{Map, ListBuffer}

import net.gumbix.analysis.DynPro._
import net.gumbix.dynpro.concurrency.Stage._

/**
 * An algorithm for data transfer.
 * Project name: scabio
 * Date: 10/2/13
 * Time: 4:30 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
class AnalyserNonActor(
  lens: ListBuffer[Long], nrOfCom: Int, dynPros: Seq[DynPro], fileName: String
) extends _Analyser{

  def start = {
    val storage = Map[DynPro, Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]()

    dynPros.foreach{dp => if(dp.isInstanceOf[DynPro]){ //dyn pro alg iteration
      storage(dp) = Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]()
      statusCounter(dp) = Map[Long, Int]()

      lens.foreach{len =>
        storage(dp)(len) = (new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double]())
        statusCounter(dp)(len) = 0

        (1 to nrOfCom).foreach{nr =>
          val timeMap = getTimeMap(dp, len)
          storage(dp)(len)._1 += timeMap._1(MATRIX) //seqMx
          storage(dp)(len)._2 += timeMap._1(TOTAL) //seqTt
          storage(dp)(len)._3 += timeMap._2(MATRIX) //conMx
          storage(dp)(len)._4 += timeMap._2(TOTAL) //conTt

          printStatus(dp, len, nr, nrOfCom)
        }
      }
    }else results(dp) = "ERROR"}

    saveStorage(lens, nrOfCom, fileName, storage)
    results
  }
}
