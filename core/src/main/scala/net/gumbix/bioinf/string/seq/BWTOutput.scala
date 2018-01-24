package net.gumbix.bioinf.string.seq

/**
  * The Burrows-Wheeler Transform.
  * @param s The string to be transformed.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class BWTOutput(s: String) extends BWT(s) {

  def rotationToLaTeX = {
    val rows = for ((i, seq) <- rotationList) yield {
      val rseq = subst(seq)
      i + " \\& \\texttt{" + rseq + "} \\\\"
    }
    "\\begin{tikzpicture}\n" +
      "\\matrix [table, matrix of nodes, ampersand replacement=\\&] {\n" +
      "$i$ \\& \\\\\n" +
      rows.mkString("\n") +
      "\n};\n\\end{tikzpicture}"
  }

  def suffixArrayToLaTeX = {
    val iters = (0 to s.size)
    val zipped = iters zip sortedRotationList
    val rows = for ((i, (sai, seq)) <- zipped) yield {
      val s1 = subst(seq(0).toString)
      val s2 = subst(seq.substring(1, seq.size - 1))
      val s3 = subst(seq(seq.size - 1).toString)
      i + " \\& " + sai + " \\& \\texttt{" +
        s1 + "} \\& \\texttt{" + s2 +
        "} \\& \\texttt{" + s3 + "} \\\\"
    }
    "\\begin{tikzpicture}\n" +
      "\\matrix [table, matrix of nodes, ampersand replacement=\\&] {\n" +
      "$s_i$ \\& $i$ \\& \\& \\& BWT \\\\\n" +
      rows.mkString("\n") +
      "\n};\n\\end{tikzpicture}"
  }

  def subst(s: String) = s.map(c => if (c == '#') "\\$" else c.toString).mkString
}