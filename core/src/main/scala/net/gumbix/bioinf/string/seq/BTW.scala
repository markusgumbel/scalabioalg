package net.gumbix.bioinf.string.seq

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class BWT(val s: String) {
  def bwt() = {
    0
  }

  val size = s.size

  val rotationList = {
    def rotate(i: Int) = s.substring(i, size) + s.substring(0, i)
    for (i <- 0 until size) yield (i, rotate(i))
  }

  val sortedRotationList = rotationList.sortBy(_._2)

  val suffixArrayList = {
    val idx = (0 until size) // Numbers 0, 1, ..., n-1
    val b = sortedRotationList.map(_._1) // Indizes.
    idx.zip(b) // Merge them.
  }

  val suffixArray = suffixArrayList.map(_._2).toArray
}

object BTW {
  def main(args: Array[String]) {
    val btw = new BWT("foobarfoba")
    println(btw.rotationList.mkString(","))
    println(btw.sortedRotationList.mkString(","))
    println(btw.suffixArrayList.mkString(","))
    // println(btw.suffixArray)
  }
}