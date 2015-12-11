package net.gumbix.bioinf.phylo

/**
  */
class TaxaMetric(val taxa: Array[String], dist: Array[Array[Double]]) {

  protected val taxonToIdx: Map[String, Int] = {
    // zip with numbers from 0 to n-1 and then map it to key-value pairs:
    taxa.zip(0 until taxa.size).toMap
  }

  def distByTaxon(taxon1: String, taxon2: String) = distByIndex(taxonToIdx(taxon1), taxonToIdx(taxon2))

  def distByIndex(i: Int, j: Int) = if (i <= j) dist(i)(j) else dist(j)(i)

  val isAdditive = false

  def mkString = {
    // TODO use MatrixPrinter
    taxa.mkString("   ") + "\n" +
      dist.map(row => row.mkString(" ")).mkString("\n")
  }
}

class NeighborJoiningMetric(taxa: Array[String], dist: Array[Array[Double]]) extends TaxaMetric(taxa, dist) {

  val njDist = {

    def r(i: Int) = {
      val d = for (k <- 0 until taxa.size) yield {distByIndex(i, k)}
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
    // TODO use MatrixPrinter
    taxa.mkString("   ") + "\n" +
      njDist.map(row => row.mkString(" ")).mkString("\n")
  }
}
