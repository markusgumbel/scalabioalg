package net.gumbix.bioinf.phylo

/**
  * Base class for all distance-based phylogenetic trees.
  *
  * @param taxa List of taxa.
  * @param dist Matrix containing the distances.
  */
class TaxaMetric(val taxa: Array[String], val dist: Array[Array[Double]])
  extends TaxaMetricPrinter {

  protected val taxonToIdx: Map[String, Int] = {
    // zip with numbers from 0 to n-1 and then map it to key-value pairs:
    taxa.zip(0 until taxa.size).toMap
  }

  /**
    * Number of taxa (n), i.e. matrix has dimension n x n.
    */
  val size = dist.length

  /**
    * Get the distance for two taxa.
    *
    * @param taxon1 First taxon.
    * @param taxon2 Second taxon.
    * @return Distance.
    */
  def distByTaxon(taxon1: String, taxon2: String)
  = distByIndex(taxonToIdx(taxon1), taxonToIdx(taxon2))

  /**
    * Get the distance for two taxa addressed by the index i and j.
    *
    * @param i First index of a taxon.
    * @param j Second index of a taxon.
    * @return Distance.
    */
  def distByIndex(i: Int, j: Int) = {
    if (i == j) {
      Double.NaN
    } else {
      if (i < j) dist(i)(j) else dist(j)(i)
    }
  }

  /**
    * True if a ultrametric tree can be constructed with this
    * distance matrix or else if not.
    */
  val isUltrametric = {
    def check(): Boolean = {
      // Iterate over all possible index combinations containing
      // 3 indices (i1, i2, i3):
      for (i1 <- 0 until size; i2 <- 0 until size if (i1 != i2);
           i3 <- 0 until size if (i1 != i3 && i2 != i3)) {
        val d12 = distByIndex(i1, i2)
        val d13 = distByIndex(i1, i3)
        val d23 = distByIndex(i2, i3)
        val ultra =
          (d12 <= d13 && d13 == d23) ||
            (d13 <= d12 && d12 == d23) ||
            (d23 <= d12 && d12 == d13)
        if (!ultra) return false // First negative, exit...
      }
      true
    }
    check()
  }

  /**
    * True if a additive tree can be constructed with this
    * distance matrix or else if not.
    */
  val isAdditive = {
    def check(): Boolean = {
      // Iterate over all possible index combinations containing
      // 4 indices (i1, i2, i3, i4):
      for (i1 <- 0 until size; i2 <- 0 until size if (i1 != i2);
           i3 <- 0 until size if (i1 != i3 && i2 != i3);
           i4 <- 0 until size if (i1 != i4 && i2 != i4 && i3 != i4)) {
        // Test for Buneman's four point condition:
        val d1 = distByIndex(i1, i2) + distByIndex(i3, i4)
        val d2 = distByIndex(i1, i3) + distByIndex(i2, i4)
        val d3 = distByIndex(i1, i4) + distByIndex(i2, i3)
        val min = List(d1, d2, d3).min
        val additive =
          (d1 == min && d2 == d3) ||
            (d2 == min && d1 == d3) ||
            (d3 == min && d1 == d2)
        if (!additive) return false // First negative, exit...
      }
      true
    }
    check()
  }
}