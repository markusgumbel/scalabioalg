package net.gumbix.bioinf.phylo

import net.gumbix.util.Logger
import org.apache.commons.math3.linear._

import scala.collection.mutable

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashTree(val metric: FitchMargoliashMetric)
  extends Logger {

  logLevel = true

  def tree() = {
    val edges = new mutable.HashMap[String, Double]()
    var m: FitchMargoliashMetric = metric
    while (m.size >= 4) {
      logln("--------------------------- iteration ---------------------------------")
      logln("Metric:")
      logln(m.mkMatrixString)

      val m3 = m.clusterNonMinDistanceTaxa()
      val im = m.minDistanceIdx(0)
      val jm = m.minDistanceIdx(1)
      logln("\nMinimum distance is dist(" + m.taxa(im) + ", " +
        m.taxa(jm) + ") = " + m.distByIndex(im, jm) + " at index " +
        m.minDistanceIdx.mkString("(", ", ", ")"))
      logln("Grouping " + m.nonMinTaxaGroup())
      logln("Metric of 3 taxa:")
      logln(m3.mkMatrixString)
      val p = calcEdges(m3)
      edges ++= p
      println("\n" + p.mkString(", "))

      logln("Reducing metric by grouping " + m.minTaxaGroup() + ".\n")
      m = m.clusterMinDistanceTaxa()
    }
    logln("--------------------------- final ---------------------------------")
    logln(m.mkMatrixString)
    val p = calcEdges(m)
    edges ++= p
    println("\n" + p.mkString(", "))
    println("\n" + edges.toList.sortBy(c => c._1).map(e => e._1 + " = " + e._2).mkString("\n"))
  }

  def calcEdges(m: FitchMargoliashMetric) = {
    val A = Array
    val a: Array[Array[Double]] = A(
      A(1d, 1, 0),
      A(1d, 0, 1),
      A(0d, 1, 1)
    )
    val coefficients = new Array2DRowRealMatrix(a, false)
    val solver = new LUDecomposition(coefficients).getSolver()
    val b = A(m.distByIndex(0, 1), m.distByIndex(0, 2), m.distByIndex(1, 2))
    val constants = new ArrayRealVector(b, false)
    val solution = solver.solve(constants)
    Map(m.taxa(0) -> solution.getEntry(0),
      m.taxa(1) -> solution.getEntry(1),
      m.taxa(2) -> solution.getEntry(2))
  }

}
