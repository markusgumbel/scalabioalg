package net.gumbix.bioinf.phylo

import net.gumbix.util.MatrixPrinter

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class NeighborJoiningMetric(taxa: Array[String], dist: Array[Array[Double]])
  extends TaxaMetric(taxa, dist) {

  val njDist = {

    def r(i: Int) = {
      val d = for (k <- 0 until taxa.size) yield {
        distByIndex(i, k)
      }
      1.0 / (taxa.size - 2) * d.sum
    }

    Array.tabulate(taxa.size, taxa.size) {
      (i, j) =>
        if (i < j) distByIndex(i, j) - (r(i) + r(j)) else 0
    }
  }

  def njDistByTaxon(taxon1: String, taxon2: String) = njDistByIndex(taxonToIdx(taxon1), taxonToIdx(taxon2))

  def njDistByIndex(i: Int, j: Int) = if (i <= j) njDist(i)(j) else njDist(j)(i)


  /**
    * The index of the minimum neighbor-joining distance.
    */
  val minIdx = {
    val valIdx = for (i <- 0 until taxa.size; j <- i until taxa.size) yield ((i, j), njDistByIndex(i, j))
    valIdx.minBy(_._2)._1 // Find minimum in distance-list and return index.
  }

  def mkNJString = {
    val these = this
    val tmp = new TaxaMetricPrinter {
      formatter = DECIMAL
      val taxa = these.taxa
      val dist = these.njDist
      val size = these.size
    }
    tmp.mkMatrixString
  }
}
