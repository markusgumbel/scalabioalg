package net.gumbix.bioinf.phylo

import net.gumbix.util.Logger
import org.apache.commons.math3.linear._

import scala.collection.mutable

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashTree(val metric: FitchMargoliashMetric)
  extends Logger {

  logLevel = false

  def tree() = {

    // Avg. distance to future nodes:
    val avgEdges = new mutable.HashMap[Taxon, Double]()
    // A sorted list of instructions how to build the tree:
    val allEdges = new mutable.ArrayBuffer[(Taxon, Double)]()

    if (metric.size >= 3) { // Otherwise there is no problem.
      def dist(taxon: Taxon, p: Map[Taxon, Double]) = taxon match {
        case jt: JoinedTaxon => {
          val avgDist = avgEdges(jt)
          val distTaxon1 = avgEdges(jt.taxa(0))
          val distTaxon2 = avgEdges(jt.taxa(1))
          val dist = avgDist - (distTaxon1 + distTaxon2) / 2
          allEdges.+=((jt, dist)) // Add inner taxon.
        }
        case _ => {
          allEdges.+=((taxon, p(taxon))) // Add leaf taxon.
        }
      }

      val allJoins = new mutable.ArrayBuffer[Taxon]()
      def join(joinedTaxa: JoinedTaxon, p: Map[Taxon, Double]) = {
        avgEdges ++= p // Add everything to the avg. edges.
        dist(joinedTaxa.taxa(0), p)
        dist(joinedTaxa.taxa(1), p)
        allJoins += joinedTaxa
      }

      var m: FitchMargoliashMetric = metric
      while (m.size >= 4) {
        logln("---------------------- iteration ---------------------------------")
        logln("Metric:")
        logln(m.mkMatrixString)

        // Calculate distances for a 3x3 matrix:
        val m3 = m.clusterNonMinDistanceTaxa()

        val im = m.minDistanceIdx(0)
        val jm = m.minDistanceIdx(1)
        logln("\nMinimum distance is dist(" + m.taxa(im) + ", " +
          m.taxa(jm) + ") = " + m.distByIndex(im, jm) + " at index " +
          m.minDistanceIdx.mkString("(", ", ", ")"))
        logln("Grouping " + m.nonMinTaxaGroup())
        logln("Metric of 3 taxa:")
        logln(m3.mkMatrixString)

        val cEdges = calcEdges(m3)
        join(m.minTaxaGroup(), cEdges)
        logln("\n" + cEdges.mkString(", "))

        logln("Reducing metric by grouping " + m.minTaxaGroup() + ".\n")
        m = m.clusterMinDistanceTaxa()
      }

      logln("--------------------------- final ---------------------------------")
      logln(m.mkMatrixString)
      val cEdges = calcEdges(m)
      logln("\n" + cEdges.mkString(", "))

      if (metric.size <= 3) {
        allEdges ++= cEdges
      } else {
        val taxon = allJoins(allJoins.size - 1)
        // Add leafs:
        allEdges ++= cEdges.filter(!_._1.isInstanceOf[JoinedTaxon])
        avgEdges.+=((taxon, cEdges(taxon)))
        dist(taxon, cEdges) // Last inner node.
        allJoins += new JoinedTaxon(Array(allJoins(allJoins.size - 1),
          m.minTaxaGroup.taxa(0), m.minTaxaGroup.taxa(1)))
      }

      logln("Edges:\n" + allEdges.sortBy(c => c._1.toString()).
        map(e => e._1 + " = " + e._2).mkString("\n"))
      logln("Tree construction (new):\n" + allJoins.mkString("\n"))
    }
    allEdges
  }

  /**
    * Calculate edges for a metric of 3 taxa by solving
    * a linear equation.
    * @param m Metric with 3 taxa.
    * @return Edge weights.
    */
  def calcEdges(m: FitchMargoliashMetric) = {
    val A = Array
    val a: Array[Array[Double]] = A(
      A(1d, 1, 0), // a + b = dist(A,B)
      A(1d, 0, 1), // a + c = dist(A,C)
      A(0d, 1, 1) // b + c = dist(B,C)
    )
    val coefficients = new Array2DRowRealMatrix(a, false)
    val solver = new LUDecomposition(coefficients).getSolver()
    val b = A(m.distByIndex(0, 1), m.distByIndex(0, 2), m.distByIndex(1, 2))
    val constants = new ArrayRealVector(b, false)
    val solution = solver.solve(constants)
    Map(m.joinedTaxa(0) -> solution.getEntry(0),
      m.joinedTaxa(1) -> solution.getEntry(1),
      m.joinedTaxa(2) -> solution.getEntry(2))
  }
}
