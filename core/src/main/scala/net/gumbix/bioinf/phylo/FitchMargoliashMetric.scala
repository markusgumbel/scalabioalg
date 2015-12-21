package net.gumbix.bioinf.phylo

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class FitchMargoliashMetric(val joinedTaxa: Array[Taxon], dist: Array[Array[Double]])
  extends TaxaMetric(joinedTaxa.map(_.toString), dist) {

  def this(taxa: Array[String], dist: Array[Array[Double]]) =
    this(taxa.map(s => new Taxon(s)), dist)

  formatter = DECIMAL

  /**
    * @return List of two indicices which have a minimal distance.
    */
  val minDistanceIdx = {
    val valIdx = for (i <- 0 until taxa.size;
                      j <- i until taxa.size if (i < j))
      yield ((i, j), distByIndex(i, j))
    // Find minimum in distance-list and return index:
    val (im, jm) = valIdx.minBy(_._2)._1
    Array(im, jm)
  }

  /**
    * @return List of all indices except those which have a minimal distance.
    */
  val nonMinDistanceIdx = {
    (0 until size).filter(!minDistanceIdx.contains(_)).toArray
  }

  /**
    * Group taxa in list idxCluster.
    * @param idxCluster
    * @return A label for cluster or group of taxa.
    */
  def taxaGroup(idxCluster: Array[Int]) = {
    new JoinedTaxon(idxCluster.map(idx => joinedTaxa(idx)))
  }

  def nonMinTaxaGroup() = taxaGroup(nonMinDistanceIdx)

  def minTaxaGroup() = taxaGroup(minDistanceIdx)

  def clusterMinDistanceTaxa() = aggregateMatrix(minDistanceIdx)

  def clusterNonMinDistanceTaxa() = aggregateMatrix(nonMinDistanceIdx)

  def aggregateMatrix(idxCluster: Array[Int]): FitchMargoliashMetric = {
    /**
      * Calculate average distance from i-th taxon to taxa
      * in list idxCluster.
      * @param i
      * @param idxCluster
      * @return
      */
    def average(i: Int, idxCluster: Array[Int]) = {
      val sum = idxCluster.map(j => distByIndex(i, j)).sum
      sum / idxCluster.size
    }

    // Quite ugly...
    if (size > 3) {
      // All rows except those contained in idxList:
      val rows = for (i <- 0 until size if (!idxCluster.contains(i))) yield {
        val columns = for (j <- 0 until size if (!idxCluster.contains(j)))
          yield distByIndex(i, j)
        val avg = average(i, idxCluster)
        val newLine = avg :: columns.toList
        newLine.toArray
      }
      // Now insert row with cluster idxs before this row array:
      val h = rows.toArray
      val firstRow: Array[Double] = Array.ofDim(size - (idxCluster.size - 1))
      firstRow(0) = Double.NaN
      for (i <- 0 until h.size) {
        firstRow(i + 1) = h(i)(0)
      }
      val newDist = (firstRow :: rows.toList).toArray
      // Create new taxa array:
      val rTaxa = for (j <- 0 until size if (!idxCluster.contains(j))) yield joinedTaxa(j)
      val newTaxa = (taxaGroup(idxCluster) :: rTaxa.toList).toArray
      new FitchMargoliashMetric(newTaxa, newDist)
    } else {
      throw new RuntimeException("Not possible for size < 3")
    }
  }
}
