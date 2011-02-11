package net.gumbix.bioinf.string.seq

import io.Source.fromFile

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

object BlockFormatter {
  val lineLength = 60

  def main(args: Array[String]) {
    val buffer = new StringBuffer
    val lines = fromFile(args(0)).getLines.drop(1)
    for (line <- lines) {
      val rLine = line.filter {
        c => c match {
          case 'A' => true
          case 'C' => true
          case 'T' => true
          case 'G' => true
          case _ => false
        }
      }
      buffer.append(rLine)
    }
    println(buffer)
    val p = buffer.length / lineLength
    for (s <- 0 until p) {
      val start = lineLength * s
      val end = Math.min(lineLength * (s + 1), buffer.length)
      val line = buffer.substring(start, end)
      println(line)
    }
  }
}