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

import net.gumbix.bioinf.string.seq._
import net.gumbix.util.Logger
import org.junit.Test
import org.junit.Assert._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class GreedySuperstringTest extends Logger {

  logLevel = false
  
  val strings = List("Markus Gumbel und Susanne Wassmuth-Gumbel",
    "Das ist eindeutiger Text, der nicht vertauscht werden kann")

  val seqs = List(
    Array("Das ist", "ist ein", "ein Text"),
    Array("ein Text", "Das is", "ist ein T"),
    Array("ein Text", "Das is", "Das ist", "ist ein T", "n Text")
    )

  @Test
  def testImp() {
    seqs.foreach {
      s =>
        val gs = new GreedySuperstringImperative(s)
        assertEquals("Das ist ein Text", gs.superstrings.mkString("|"))
    }
  }

  @Test
  def testFct() {
    seqs.foreach {
      s =>
        val gs = new GreedySuperstringFct(s)
        gs.logLevel = false
        assertEquals("Das ist ein Text", gs.superstrings.mkString("|"))
    }
  }

  @Test
  def testBig() {
    strings.foreach {
      s =>
        val fragments = new Fragments(s, 10, 10, 1).fragments.toArray

        logln("Fragments:")
        logln(fragments.mkString(", \n"))
        logln()

        val gs = new GreedySuperstringFct(fragments) {
          logLevel = false
        }
        // val gs = new GreedySuperstringImperative(fragments)
        assertEquals(s, gs.superstrings.mkString("|"))
    }
  }

  @Test
  def testOverlap() {
    val gs = new Overlap {}
    assertEquals("ba", gs.overlap("aba", "baca"))
    assertEquals("", gs.overlap("aba", "caca"))
    assertEquals("abab", gs.overlap("abab", "abab"))
    assertEquals("ab", gs.overlap("ab", "aba"))
    assertEquals("ab", gs.overlap("aab", "abaaaa"))
  }

  @Test
  def testFragmentation() {
    println("Fragmentation")
    val f = new Fragments("Markus Gumbel und Susanne Wassmuth-Gumbel",
      10, 10, 0)
    assertEquals("Markus Gumbel und ",f.fragments(0))
    logln(f.fragments.mkString(", "))
  }
}