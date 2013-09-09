package net.gumbix.string.alignment

import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.concurrency.Debugger
import java.lang.management.{ManagementFactory, ThreadMXBean}
import scala.util.Random

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/18/13
 * Time: 3:17 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
class ConAlignment(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
  override val config = setConfig(LEFT_UP, EVENT)
  override val values = Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)
}

object DebugMultAlignApp{
  def main(args: Array[String]) {
    val (len, lim, aas, s1, s2) = (
      500, 500, List('A', 'C', 'G', 'T'), new StringBuilder, new StringBuilder
      )
    val man = ManagementFactory.getThreadMXBean

    (0 until len).foreach{i =>
      s1 += Random.shuffle(aas).head
      s2 += Random.shuffle(aas).head
    }

    (0 until lim).foreach {i =>
      print("Round %s/%s =>  Peak thread Count = %s,".format(i+1, lim, man.getPeakThreadCount))
      new ConAlignment(s1.mkString , s2.mkString).solution

      Debugger.printMemories //memory usage
    }
    println("Done")

    /*
    var tGroup = Thread.currentThread().getThreadGroup
    while(tGroup.getParent != null) tGroup = tGroup.getParent
    val allGroups = Array.ofDim[ThreadGroup](1000)
    val i = tGroup.enumerate(allGroups, true)
    */


  }

  //Results so far: problem origin unknown
  // Once it starts running, it runs till the end.
  // Unfortunately much often than required the first iteration doesn't terminated.
}
