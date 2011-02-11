/*
Copyright 2011 the original author or authors.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
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