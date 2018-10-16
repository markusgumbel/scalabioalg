package net.gumbix.bioinf.string.alignment

import scala.collection.immutable.{HashMap, HashSet}

/**
  * A manually created multiple alignment.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class MultipleAlignment(msa: Array[AlignedString])
  extends AbstractMultipleAlignment(msa.map(_.primaryString)) {

  val multipleAlignment = {
    val sizes = new HashSet() ++ msa.map(_.size)
    if (sizes.size != 1) {
      throw new IllegalArgumentException("Aligned strings must be " +
        "of the same size. Sizes are " + sizes.mkString(", "))
    }
    msa
  }
}
