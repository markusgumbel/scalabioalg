package net.gumbix.analysis.demo

import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.concurrency.Debugger

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/18/13
 * Time: 3:17 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
class ConAlignment(s1: String, s2: String) extends
Alignment(new StringBuilder(s1), new StringBuilder(s2), AlignmentMode.GLOBAL){
  override val config = setConfig(LEFT_UP, EVENT, true)
  override val values = Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)
}

object DebugMultAlignApp{
  def main(args: Array[String]) {
    val (lim, s1, s11, s2) = //
      (100, "wiesengrund",
        "wiesengrund lampe bringen Meier Messer Ziele Straßburgwiesengrund lampe bringen Meier Messer Ziele Straßburgwiesengrund lampe bringen Meier Messer Ziele Straßburgwiesengrund lampe bringen Meier Messer Ziele Straßburg",
        "schweinehund ampel trinken Maier Metzger Zeile Strassbourg")

    val alignment = new ConAlignment(s11, s2)
    (0 until lim).foreach {
      case i => {
        print("Round " + (i+1) + "/" + lim)
        Debugger.printMemories
        alignment.solution
        print("\n")
      }
    }
    println("Done")
  }

  //Results so far: problem origin unknown
  // Once it starts running, it runs till the end.
  // Unfortunately much often than required the first iteration doesn't terminated.
}
