package net.gumbix.bioinf.phylo

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class NeighborJoiningMetric(joinedTaxa: Array[Taxon], dist: Array[Array[Double]])
  extends JoinedTaxaMetric[NeighborJoiningMetric](joinedTaxa, dist) {

  def this(taxa: Array[String], dist: Array[Array[Double]]) =
    this(taxa.map(s => new Taxon(s)), dist)

  val r = {
    val h = for (i <- 0 until taxa.length) yield {
      val d = for (k <- 0 until taxa.size if (k != i)) yield {
        distByIndex(i, k)
      }
      1.0 / (taxa.size - 2) * d.sum
    }
    h.toArray
  }

  def rByTaxon(taxon: String) = r(taxonToIdx(taxon))

  val njDist = Array.tabulate(taxa.size, taxa.size) {
    (i, j) =>
      if (i < j) distByIndex(i, j) - (r(i) + r(j)) else 0
  }

  def njDistByTaxon(taxon1: String, taxon2: String) =
    njDistByIndex(taxonToIdx(taxon1), taxonToIdx(taxon2))

  def njDistByIndex(i: Int, j: Int) = if (i <= j) njDist(i)(j) else njDist(j)(i)

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

  def joinDistByIdx(i: Int, j: Int) = njDistByIndex(i, j)

  /**
    * Calculate average distance from i-th taxon to taxa
    * in list idxCluster.
    * @param k
    * @param idxCluster
    * @return
    */
  def average(k: Int, idxCluster: Array[Int]) = {
    if (idxCluster.length == 2) {
      val i = idxCluster(0)
      val j = idxCluster(1)
      0.5 * (distByIndex(i, k) + distByIndex(j, k) - distByIndex(i, j))
    } else {
      throw new RuntimeException("Neighbor joining alg. expects cluster of size 2.")
    }
  }

  def newJoinedMetric(joinedTaxa: Array[Taxon], dist: Array[Array[Double]]) = {
    new NeighborJoiningMetric(joinedTaxa, dist)
  }
}
