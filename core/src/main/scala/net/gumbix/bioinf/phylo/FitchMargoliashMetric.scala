package net.gumbix.bioinf.phylo

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashMetric(joinedTaxa: Array[Taxon], dist: Array[Array[Double]])
  extends JoinedTaxaMetric[FitchMargoliashMetric](joinedTaxa, dist) {

  def this(taxa: Array[String], dist: Array[Array[Double]]) =
    this(taxa.map(s => new Taxon(s)), dist)

  /**
    * The original distance matrix is used for the
    * calculation of the minimum distances.
    * @param i
    * @param j
    * @return
    */
  def joinDistByIdx(i: Int, j: Int) = distByIndex(i, j)

  /**
    * The average from i to all taxa in idxCluster.
    * @param i
    * @param idxCluster
    * @return
    */
  def average(i: Int, idxCluster: Array[Int]) = {
    val sum = idxCluster.map(j => distByIndex(i, j)).sum
    sum / idxCluster.size
  }

  def newJoinedMetric(joinedTaxa: Array[Taxon], dist: Array[Array[Double]]) = {
    new FitchMargoliashMetric(joinedTaxa, dist)
  }
}
