package net.gumbix.analysis

import actors.{Actor, TIMEOUT}
import collection.mutable.{ListBuffer, Map}

import net.gumbix.analysis.DynPro._
import net.gumbix.analysis.Db._
import net.gumbix.dynpro.concurrency.Stage._
import net.gumbix.dynpro.concurrency.Messages._

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 9/15/13
 * Time: 11:22 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This actor concurrently runs the analysis of ALL the given dyn pro alg's.
 * @see The "conRun" method below to find out the purpose of each parameter.
 */
private class AnalyserActor(
  lens: (ListBuffer[Long], ListBuffer[Long], ListBuffer[Long]),
  nrOfCom: Int, dynPros: Seq[DynPro], fileName: String
) extends Actor with _Analyser{

  def act{react{case START =>
    val (to, storage) = ( //the storage in which the values obtained during one round are stored before their evaluation.
      sender,
      Map[DynPro, Map[Long, (ListBuffer[Double], ListBuffer[Double], ListBuffer[Double], ListBuffer[Double])]]()
      //the storage dp -> len -> (current) nrOfCom -> (seqMx, seqTt, conMx, conTt)
    )
    lazy val andThenBlock = { //used once the loopWhile is done
      saveStorage(lens._1, nrOfCom, fileName, storage) //the results object is updated in this method
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
        printStatus(dp, len, nr, nrOfCom)
        loopStart -= 1
      case TIMEOUT => println(anaDate + "The Analyser is still WAITING for the results!")
        //This is the 3rd of 5 milestones
    }}andThen andThenBlock
  }}

}


/**
 * This actor is used (by extension) to concurrently conRun the sequential- and concurrent modes of a chosen dyn pro alg.
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
