package net.gumbix.bioinf.string.seq

/**
  * The Burrows-Wheeler Transform.
  * @param s The string to be transformed.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class BWT(val s: String) {

  val rotationList = {
    def rotate(i: Int) = s.substring(i, s.size) + s.substring(0, i)
    for (i <- 0 until s.size) yield (i, rotate(i))
  }

  val sortedRotationList = rotationList.sortBy(_._2)

  val suffixArrayList = {
    val idx = (0 until s.size) // Numbers 0, 1, ..., n-1
    val b = sortedRotationList.map(_._1) // Indizes.
    idx.zip(b) // Merge them.
  }

  val suffixArray = suffixArrayList.map(_._2).toArray

  val transformation = {
    val bwtList = sortedRotationList.map(_._2).map(s => s(s.size - 1))
    bwtList.mkString("")
  }
}

object BTW {
  def main(args: Array[String]) {
    //val btw = new BWT("^BANANA|")
    val btw = new BWT("googol^")
    println("rotation list:")
    println(btw.rotationList.mkString(","))
    println("sorted rotation list:")
    println(btw.sortedRotationList.mkString(","))
    println("suffix array (list):")
    println(btw.suffixArrayList.mkString(","))
    println("BWT:")
    println(btw.transformation)
  }
}