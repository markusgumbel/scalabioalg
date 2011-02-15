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
package net.gumbix.bioinf.string.seq.test

import junit.framework.TestCase
import net.gumbix.bioinf.string.seq.{Fragments, GreedySuperstringFct, GreedySuperstringImperative, GreedySuperstring}
import net.gumbix.util.Logger

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class SeqTest extends TestCase with Logger {

  logLevel = false
  
  val strings = List("Markus Gumbel und Susanne Wassmuth-Gumbel",
    "Das ist eindeutiger Text, der nicht vertauscht werden kann")

  val seqs = List(
    Array("aba", "baaaa", "aab"),
    Array("Das ist", "ist ein", "ein Text"),
    Array("ein Text", "Das is", "ist ein T"),
    Array("ein Text", "Das is", "Das ist", "ist ein T", "n Text")
    )

  def testImp() {
    seqs.foreach {
      s =>
        println("Alignment")
        val gs = new GreedySuperstringImperative(s)
        println(s.mkString(",") + " = '" + gs.superstrings.mkString("|") + "'")
    }
  }

  def testFct() {
    seqs.foreach {
      s =>
        println("Alignment")

        val gs = new GreedySuperstringFct(s)
        println(s.mkString(",") + " = '" + gs.superstrings.mkString("|") + "'")
    }
  }

  def testBig() {
    strings.foreach {
      s =>
        println("\n*** Alignment ***")
        val fragments = new Fragments(s, 10, 10).fragments.toArray

        logln("Fragments:")
        logln(fragments.mkString(", \n"))
        logln()

        val gs = new GreedySuperstringFct(fragments) {
          logLevel = false
        }
        // val gs = new GreedySuperstringImperative(fragments)
        println("\n'" + s + "' = '" + gs.superstrings.mkString("|") + "'")
    }
  }

  def testOverlap() {
    println("Overlap")
    val gs = new GreedySuperstringImperative(Array[String]("aba", "baaaa", "aab"))

    val pairs = List(("aba", "baca"),
      ("aba", "caba"),
      ("abab", "abab"),
      ("ab", "aba"),
      ("aab", "abaaaa")
      )
    pairs.foreach {
      pair =>
        println(pair._1 + " _ " + pair._2 + " = " + gs.overlap(pair._1, pair._2))
    }
  }

  def testFragmentation() {
    println("Fragmentation")
    val f = new Fragments("Markus Gumbel und Susanne Wassmuth-Gumbel",
      10, 10)
    println(f.fragments.mkString(", "))
  }
}