package net.gumbix.bioinf.phylo

import net.gumbix.util.MatrixPrinter

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait TaxaMetricPrinter extends MatrixPrinter {

  val taxa: Array[Taxon]
  val dist: Array[Array[Double]]

  val size: Int

  formatter = INT

  lazy val matrix = {
    val m: Array[Array[Option[Double]]] = Array.ofDim(size - 1, size - 1)
    for (i <- 0 until size - 1; j <- 1 until size) {
      m(i)(j - 1) = if (j > i) Some(dist(i)(j)) else None
    }
    m
  }

  override def columnCounter =
    (0 until matrix(0).length).map(c => (c + 1).toString).toArray

  /**
    * Labels are the taxa.
    */
  override def columnLabels = Some(taxa.takeRight(size - 1).map(_.toString))

  /**
    * Labels are the taxa.
    */
  override def rowLabels = Some(taxa.take(size - 1).map(_.toString))
}
