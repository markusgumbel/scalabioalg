package net.gumbix.bioinf

/**
 * Demo of using BioJava from scala code. Performs a simple sequence alignment.
 *
 * This shows off a few nice scala features, such as implicit methods
 */

import org.biojava3.alignment.{Alignments,SimpleGapPenalty,SubstitutionMatrixHelper}
import org.biojava3.alignment.Alignments.PairwiseSequenceAlignerType.LOCAL
import org.biojava3.core.sequence.DNASequence
import org.biojava3.core.sequence.compound.AmbiguityDNACompoundSet

object PSA_DNA {
  implicit def str2DNA(seq: String) = new DNASequence(seq,AmbiguityDNACompoundSet.getDNACompoundSet)

  def main(args: Array[String]) {
    // Note implicit cast from strings to Ambiguous DNASequence
    val target: DNASequence = "CACGTTTCTTGTGGCAGCTTAAGTTTGAATGTCATTTCTTCAATGGGACGGA"+
      "GCGGGTGCGGTTGCTGGAAAGATGCATCTATAACCAAGAGGAGTCCGTGCGCTTCGACAGC"+
      "GACGTGGGGGAGTACCGGGCGGTGACGGAGCTGGGGCGGCCTGATGCCGAGTACTGGAACA"+
      "GCCAGAAGGACCTCCTGGAGCAGAGGCGGGCCGCGGTGGACACCTACTGCAGACACAACTA"+
      "CGGGGTTGGTGAGAGCTTCACAGTGCAGCGGCGAG"

    val query: DNASequence = "ACGAGTGCGTGTTTTCCCGCCTGGTCCCCAGGCCCCCTTTCCGTCCTCAGGAA"+
      "GACAGAGGAGGAGCCCCTCGGGCTGCAGGTGGTGGGCGTTGCGGCGGCGGCCGGTTAAGGT"+
      "TCCCAGTGCCCGCACCCGGCCCACGGGAGCCCCGGACTGGCGGCGTCACTGTCAGTGTCTT"+
      "CTCAGGAGGCCGCCTGTGTGACTGGATCGTTCGTGTCCCCACAGCACGTTTCTTGGAGTAC"+
      "TCTACGTCTGAGTGTCATTTCTTCAATGGGACGGAGCGGGTGCGGTTCCTGGACAGATACT"+
      "TCCATAACCAGGAGGAGAACGTGCGCTTCGACAGCGACGTGGGGGAGTTCCGGGCGGTGAC"+
      "GGAGCTGGGGCGGCCTGATGCCGAGTACTGGAACAGCCAGAAGGACATCCTGGAAGACGAG"+
      "CGGGCCGCGGTGGACACCTACTGCAGACACAACTACGGGGTTGTGAGAGCTTCACCGTGCA"+
      "GCGGCGAGACGCACTCGT"

    val matrix = SubstitutionMatrixHelper.getNuc4_4()

    val gapP = new SimpleGapPenalty()
    gapP.setOpenPenalty(5)
    gapP.setExtensionPenalty(2)

    val psa = Alignments.getPairwiseAlignment(query, target, LOCAL, gapP, matrix)

    println(psa)
  }
}


