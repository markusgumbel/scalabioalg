package net.gumbix.bioinf

import org.biojava3.core.sequence.DNASequence
import string.alignment.Alignment
import string.alignment.AlignmentMode._
import java.net.URL
import org.biojava3.core.sequence.io.FastaReaderHelper

object DNASeq {
  implicit def seq2string(seq: DNASequence) = seq.toString

  def main(args: Array[String]) {
    val s1 = new DNASequence("AGTC")
    val s2 = new DNASequence("AGATTC")
    println(s1)

    val align = new Alignment(s1, s2, GLOBAL)
    println(align.mkMatrixString)

    // AAI44185: Homo sapiens (human) NR1H4 protein
    // AAL57619: Gallus gallus (chicken) partial orphan nuclear receptor FXR
    val fxr1 = getEMBLBank("AAI44185").values.iterator.next
    val fxr2 = getEMBLBank("AAL57619").values.iterator.next
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