package net.gumbix.bioinf.phylo

import scala.collection.mutable

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait PhyloTree {

  def tree(): mutable.ArrayBuffer[(Taxon, Double)]
}
