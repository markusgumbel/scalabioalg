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
package net.gumbix.bioinf.string.alignment.demo

import junit.framework.TestCase
import net.gumbix.bioinf.string.alignment.{MultipleAlignment, AlignmentMode}
import net.gumbix.bioinf.string.alignment.AlignmentMode._

class MultipleAlignmentDemo extends TestCase {
  val strings: Array[Tuple2[Array[String], String]] = Array(
    (Array("ATGCATT", "AGTCAAT", "TCTCA"), "Böckenhauer, p. 107"),
    (Array("KYFHKAGNQHSPT", "KYFHKAGNGHT", "KEFHNGHT"), "Hütt, p. 187"),
    (Array("123", "234", "345"), "Test1"),
    (Array("123", "234", "345", "456"), "Test2"),
    (Array("123", "345", "567"), "Test3"),
    (Array("AATGCT", "ATTC", "TCC"), "Folienbsp. Consensus"),
    (Array("NYLS", "NFLS", "NKYLS", "NFS"), "Folienbsp. MSA"),
    (Array("GCTTATA", "GCTATA", "GTTATA", "GCTTAGA"), "Übung MSA")
    )

  def testMultipleAlignment() {
    for (s <- strings; if (s._2.startsWith("Übung MSA")); mode <- AlignmentMode.values) {
      doMultipleAligment(s._1, mode, s._2)
    }
  }

  def doMultipleAligment(s: Array[String], mode: AlignmentMode, comment: String) {
    println("---------------------------------")
    println("Multiple Alignment, mode = " + mode + ", " + comment)
    println()
    (0 until s.size).foreach {i => print("s" + i + " = '" + s(i) + "', ")}
    println()
    println()
    val ma = new MultipleAlignment(s, mode)
    println(ma.mkString)
    // ma.printAlignment
    println()
    println("dist = " + ma.distance)
    println("SP = " + ma.sumOfPairs)
    // println("root index = " + ma.rootIdx)
    println()
  }
}