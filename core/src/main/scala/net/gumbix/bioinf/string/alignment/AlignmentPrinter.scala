package net.gumbix.bioinf.string.alignment

import net.gumbix.layout.Element._

/**
  * A base class for printing alignments to console or LaTeX/Tikz.
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
trait AlignmentPrinter {

  /**
    * Creates an alignment for console output.
    *
    * @param als1 The first aligned string.
    * @param als2 The second aligned string.
    * @return Two strings and a line inbetween.
    */
  def makeAlignmentString(als1: AlignedString, als2: AlignedString): String = {
    val separator = for (i <- 0 until als1.size) yield {
      if (als1.isGapAt(i) || als2.isGapAt(i)) " "
      else if (als1(i) == als2(i)) "|" else " "
    }

    // TODO some kind of global variable, so we need to set the alignment
    val s = line(als1.toString, 0) above
      line(separator.mkString, 0) above
      line(als2.toString, 0)
    s.toString
  }

  /**
    * Creates an alignment for LaTeX includes.
    *
    * @param als1 The first aligned string.
    * @param als2 The second aligned string.
    * @return Two strings and a line inbetween.
    */
  def makeLaTeXAlignmentString(als1: AlignedString, als2: AlignedString,
                               label1: String = "$s_1$",
                               label2: String = "$s_2$"): String = {
    require(als1.size == als2.size)

    def seq2LaTeXLine(als: AlignedString) = {
      val s = als.toString
      val l = s.toCharArray
      l.mkString("  ", " \\& ", " \\\\\n")
    }

    var s = "\\begin{tikzpicture}\n" +
      // Style alignment is defined in BIM\bim_preamble.tex
      "\\matrix (matrix) [matrix of nodes,ampersand replacement=\\&,alignment] {\n"

    s += seq2LaTeXLine(als1)

    val separator = for (i <- 0 until als1.size) yield {
      if (als1.isGapAt(i) || als2.isGapAt(i)) " "
      else if (als1(i) == als2(i)) "\\textcolor{gray}{$\\vert$}" else " "
    }
    s += separator.mkString("  ", " \\& ", " \\\\\n")

    s += seq2LaTeXLine(als2)

    s += "};\n"

    s += "\\node[left=2mm of matrix-1-1] {" + label1 + "};\n" +
      "\\node[left=2mm of matrix-3-1] {" + label2 + "};\n"
    s += "\\end{tikzpicture}"
    s
  }
}
