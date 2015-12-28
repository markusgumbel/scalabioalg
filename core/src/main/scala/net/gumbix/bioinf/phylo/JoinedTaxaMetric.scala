package net.gumbix.bioinf.phylo

/**
  * A metric that enables the creation of an aggregated
  * metric which averaged values.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
abstract class JoinedTaxaMetric[T](val joinedTaxa: Array[Taxon],
                                dist: Array[Array[Double]])
  extends TaxaMetric(joinedTaxa.map(_.toString), dist) {

  /**
    * Alternative constructor for compatibility with
    * the plain TaxaMetric constructor.
    * @param taxa List of taxa.
    * @param dist Distance matrix.
    * @return
    */
  def this(taxa: Array[String], dist: Array[Array[Double]]) =
    this(taxa.map(s => new Taxon(s)), dist)

  formatter = DECIMAL

  /**
    * Defines how or with with metrix the minimum taxa can be
    * identified.
    * @param i
    * @param j
    * @return
    */
  def joinDistByIdx(i: Int, j: Int): Double

  /**
    * @return List of two indices which have a minimal distance.
    *         The joinDistByIdx() method is used.
    */
  lazy val minDistanceIdx = {
    val valIdx = for (i <- 0 until taxa.size;
                      j <- i until taxa.size if (i < j))
      yield ((i, j), joinDistByIdx(i, j))
    // Find minimum in distance-list and return index:
    val (im, jm) = valIdx.minBy(_._2)._1
    Array(im, jm)
  }

  /**
    * @return List of all indices except those which have a minimal distance.
    */
  lazy val nonMinDistanceIdx = {
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

  /**
    * Calculate average distance from i-th taxon to taxa
    * in list idxCluster.
    * @param i
    * @param idxCluster
    * @return
    */
  def average(i: Int, idxCluster: Array[Int]): Double

  /**
    * Create a joined metric.
    * @param joinedTaxa
    * @param dist
    * @return
    */
  def newJoinedMetric(joinedTaxa: Array[Taxon], dist: Array[Array[Double]]): T

  def aggregateMatrix(idxCluster: Array[Int]): T = {
    // Quite ugly...
    if (size > 2) {
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
      newJoinedMetric(newTaxa, newDist)
    } else {
      throw new RuntimeException("Not possible for size <= 2")
    }
  }
}

