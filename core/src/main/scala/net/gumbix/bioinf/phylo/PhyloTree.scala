package net.gumbix.bioinf.phylo

import net.gumbix.util.Logger

import scala.collection.mutable

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait PhyloTree extends Logger {

  // A sorted list of instructions how to build the tree:
  val allEdges = new mutable.ArrayBuffer[(Taxon, Double)]()
  // A sorted list to give instructions how to build the tree:
  val allJoins = new mutable.ArrayBuffer[Taxon]()

  // now allEdges

  def showSolution() {
    logln("\n###")
    logln("allEdges")
    logln(allEdges.mkString("\n"))
    logln("allJoins")
    logln(allJoins.mkString("\n"))
    logln("###")
    logln("\nEdges (sorted):\n" + allEdges.toArray.sortBy(c => c._1.toString()).
      map(e => e._1 + " = " + e._2).mkString("\n"))
    logln("\nTree construction algorithm:")
    val allMapEdges = allEdges.toMap
    for (j <- allJoins) {
      val jt = j.asInstanceOf[JoinedTaxon]
      val t1 = jt.taxa(0)
      val t2 = jt.taxa(1)
      val w1 = allMapEdges(t1)
      val w2 = allMapEdges(t2)
      logln(" Connect nodes " + t1 + " and " + t2 +
        " with weights " + t1 + " = " + w1 + ", " + t2 + " = " + w2 +
        " to an inner node " + jt + ".")
    }
    // One remaining node:
    val tf = allEdges(allEdges.size - 1)._1
    val wf = allMapEdges(tf)
    logln(" Connect remaining node " + tf +
      " with weight " + tf + " = " + wf +
      " to an inner node " + allJoins(allJoins.size - 1) + ".")
  }
}
