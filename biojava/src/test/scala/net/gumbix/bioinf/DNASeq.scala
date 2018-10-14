package net.gumbix.bioinf


import java.net.URL

import net.gumbix.bioinf.string.alignment.Alignment
import net.gumbix.bioinf.string.alignment.AlignmentMode._
import org.biojava.nbio.core.sequence.DNASequence
import org.biojava.nbio.core.sequence.io.FastaReaderHelper


object DNASeq {
  implicit def seq2string(seq: DNASequence) = seq.toString

  def main(args: Array[String]) {
    val s1 = new DNASequence("AGTC")
    val s2 = new DNASequence("AGATTC")
    println(s1)

    val align = new Alignment(s1, s2, GLOBAL)
    println(align.mkMatrixString)

    // BC144184: Homo sapiens (human) NR1H4 protein
    // AF456453: Gallus gallus (chicken) partial orphan nuclear receptor FXR
    val fxr1 = getEMBLBank("BC144184").values.iterator.next
    val fxr2 = getEMBLBank("AF456453").values.iterator.next
    val alignFxr = new Alignment(fxr1, fxr2, GLOBAL)
    //println(alignFxr.mkMatrixString)
    val solution = alignFxr.solution
    println("sim = " + alignFxr.similarity)
    println(alignFxr.makeAlignmentString(solution))


    // val nw = new NeedlemanWunsch[Sequence[DNA], ]
  }

  def getEMBLBank(accId: String) = {
    val url = new URL("http://www.ebi.ac.uk/ena/data/view/" +
            accId + "&display=fasta")
    val ios = url.openStream()
    val map = FastaReaderHelper.readFastaDNASequence(ios)
    // println(map.values)
    ios.close()
    map
  }
}