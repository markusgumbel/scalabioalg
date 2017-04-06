package net.gumbix.bioinf.string.seq

/**
  * The inverse Burrows-Wheeler Transform.
  * Begin of string is "^" and end of string is "|".
  * @param r Burrows-Wheeler transformed text.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class BWTInv(val r: String) {

  val transformation = {
    var l = r.map(c => "").toList
    for (i <- 0 to r.size) {
      l = (r.map(c => c.toString) zip l).map(e => e._1 + e._2).toList
      println(l)
      l = l.sortBy(s => s)
      println(l)
      println("")
    }
    val f = l.filter(s => s(r.size - 1) == '|')
    f.head.substring(0, r.size)
  }
}
