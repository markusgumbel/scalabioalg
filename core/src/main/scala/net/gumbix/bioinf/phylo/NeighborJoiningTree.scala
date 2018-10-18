package net.gumbix.bioinf.phylo

import net.gumbix.util.Logger

import scala.collection.mutable

/**
  * The Neighbor-Joining algorithm, adapted from
  * Durbin et al. "Biological Sequence Analysis" (1998), p. 172.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class NeighborJoiningTree(val metric: NeighborJoiningMetric,
                          ll: Boolean = false)
  extends PhyloTree with Logger {

  logLevel = ll

  var m = metric

  def logIteration() {
    logln("---------------------- iteration ---------------------------------")
    logln("Metric:")
    logln(m.mkMatrixString)
    logln("Normalized Metric:")
    logln(m.mkNJString)

    val im = m.minDistanceIdx(0)
    val jm = m.minDistanceIdx(1)
    logln("\nMinimum distance is dist(" + m.taxa(im) + ", " +
      m.taxa(jm) + ") = " + m.distByIndex(im, jm) + " at index " +
      m.minDistanceIdx.mkString("(", ", ", ")"))
    logln("Reducing metric by grouping " + m.minTaxaGroup() + ".\n")
  }

  def logFinal() {
    logln("--------------------------- final ---------------------------------")
    logln("Metric:")
    logln(m.mkMatrixString)
    logln("Normalized Metric:")
    logln(m.mkNJString)
  }

  def join(joinedTaxa: JoinedTaxon) {
    val taxon1 = joinedTaxa.taxa(0)
    val taxon2 = joinedTaxa.taxa(1)
    val t1 = taxon1.name
    val t2 = taxon2.name
    val dist1 =
      0.5 * (m.distByTaxon(t1, t2) + m.rByTaxon(t1) - m.rByTaxon(t2))
    val dist2 = m.distByTaxon(t1, t2) - dist1
    allJoins += joinedTaxa
    allEdges.+=((taxon1, dist1))
    allEdges.+=((taxon2, dist2))
  }

  while (m.size >= 3) {
    logIteration()
    join(m.minTaxaGroup()) // Join the taxa with the minimal distance.
    m = m.clusterMinDistanceTaxa()
  }

  if (metric.size > 2) {
    val taxon = if (m.joinedTaxa(0).size == 1) m.joinedTaxa(0) else m.joinedTaxa(1)
    allEdges.+=((taxon, m.distByIndex(0, 1)))
  }
  logFinal()
  showSolution()
}

