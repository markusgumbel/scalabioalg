package net.gumbix.bioinf.string.alignment

import net.gumbix.bioinf.phylo.{JoinedTaxon, NeighborJoiningMetric, NeighborJoiningTree, Taxon}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class Clustal(strings: Array[String], ll: Boolean = false)
  extends MultipleAlignment(strings)
    with ProgressiveAlignment {

  logLevel = ll
  private val maMap = new mutable.HashMap[Taxon, List[AlignedString]]

  /**
    * Calculate the distance metrics for the neighbor joining
    * algorithm.
    * TODO Warning: Side effect as maMap is also created.
    */
  val distMetric = {
    val taxa = new ArrayBuffer[Taxon]()
    // Create some id for the sequences.
    for (i <- 0 until strings.size) {
      val taxon = new Taxon(i.toString)
      val as = new AlignedString(strings(i))
      maMap.put(taxon, List(as))
      taxa += taxon
    }
    val m = Array.ofDim[Double](alignments.size, alignments.size)
    for (i <- 0 until alignments.size; j <- 0 until alignments.size) {
      m(i)(j) = alignments(i)(j).similarity
    }
    // TODO improve distance matrix:
    val maximum = m.flatten.max // Max. value in scores.
    for (i <- 0 until alignments.size; j <- 0 until alignments.size) {
      m(i)(j) = maximum - m(i)(j)
    }
    new NeighborJoiningMetric(taxa.toArray, m)
  }

  val multipleAlignment = {

    /**
      * Insert the gaps in all aligned strings of a MSA.
      * @param msa
      * @param gaps
      */
    def insertGapsInMSA(msa: List[AlignedString], gaps: List[Int]) {
      for (as <- msa) {
        insertGaps(as, gaps.reverse)
      }
    }

    /**
      * Align two sequences or multiple alignments or
      * any combination of both.
      * @param t Taxon indicating either a single sequence
      *          or a nested taxon referring to two taxa.
      */
    def align(t: Taxon) {
      val jt = t.asInstanceOf[JoinedTaxon]
      val t1 = jt.taxa(0)
      val t2 = jt.taxa(1)
      // We can ensure that the taxa are available:
      val msa1 = maMap.get(t1).get
      val msa2 = maMap.get(t2).get
      val c1 = consensusFromList(msa1)
      val c2 = consensusFromList(msa2)
      val a = new Alignment(c1, c2, mode)
      val (as1, as2) = a.alignedStrings()
      val (ins1, ins2) = getInsertions(as1, as2)
      insertGapsInMSA(msa1, ins2) // Insert gaps in the other MSA.
      insertGapsInMSA(msa2, ins1)
      val msa = msa1 ::: msa2 // join both (multiple) alignments
      maMap.put(t, msa)
    }

    val m = new NeighborJoiningTree(distMetric)
    // Go through all sequences:
    for (t <- m.allJoins) {
      align(t)
    }
    // One remaining node:
    val ft1 = m.allEdges(m.allEdges.size - 1)._1
    val ft2 = m.allJoins(m.allJoins.size - 1)
    val jt = new JoinedTaxon(List(ft1, ft2).toArray)
    align(jt)
    maMap(jt).toArray
  }
}
