package net.gumbix.bioinf.phylo

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class Taxon(val name: String) {

  val size = 1

  override def toString() = name

  override def equals(o: Any) = {
    if (o == null || !o.isInstanceOf[Taxon]) {
      false
    } else {
      val taxon = o.asInstanceOf[Taxon]
      name == taxon.name
    }
  }

  override def hashCode() = name.hashCode
}

class JoinedTaxon(val taxa: Array[Taxon]) extends Taxon("") {

  override val name = toString()

  override val size = taxa.length

  override def toString() = taxa.mkString("{", ", ", "}")
}
