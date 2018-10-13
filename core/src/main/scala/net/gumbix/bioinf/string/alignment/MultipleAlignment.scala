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
package net.gumbix.bioinf.string.alignment

import net.gumbix.bioinf.string.alignment.GapType._
import net.gumbix.util.Logger
import org.apache.commons.math3.stat.Frequency

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
abstract class MultipleAlignment(val strings: Array[String])
  extends MultipleAlignmentPrinter
    with Score with Logger {
  logLevel = true

  val mode = AlignmentMode.GLOBAL

  /**
    * Calculate the consensus sequence.
    */
  protected def consensusFromList(seqs: List[AlignedString]) = {
    assert(!seqs.isEmpty)
    val m = seqs.size

    if (m == 1) {
      seqs(0).toString()
    } else {
      val columns = seqs(0).size
      val c = for (j <- 0 until columns) yield {
        val histo = new Frequency
        // TODO quite ugly
        val symbols = seqs.map(s => s(j)).
          filter(c => c != GAP.toString.apply(0) &&
            c != EMPTY.toString.apply(0))
        symbols.foreach(histo.addValue)
        var max = 0L
        var cSymbol = ' '
        for (s <- symbols) {
          if (histo.getCount(s) > max) {
            max = histo.getCount(s)
            cSymbol = s
          }
        }
        cSymbol
      }
      c.toList.mkString
    }
  }

  /**
    * Calculate the consensus sequence.
    */
  def consensus = consensusFromList(multipleAlignment.toList)

  /**
    * @return The number of differences column-wise between the
    *         consensus string the and the aligned strings.
    */
  def distance = {
    // Nested list of all differences
    (0 until consensus.size).flatMap {
      j =>
        // List of differences for column j
        multipleAlignment.map(t => t(j)).filter(_ != consensus(j))
    }.size
  }

  /**
    * @return The similarity of this alignment
    *         using the sum of pairs method.
    */
  def sumOfPairs = {
    val n = multipleAlignment.size
    val m = multipleAlignment(0).size
    val columns = for (j <- 0 until m) yield {
      for (i1 <- 0 until n; i2 <- i1 + 1 until n) yield {
        val c1 = multipleAlignment(i1)(j)
        val c2 = multipleAlignment(i2)(j)
        score(c1, c2)
      }
    }
    val sp = columns.flatten // flatten nested lists
    sp.reduceLeft(_ + _)
  }

  /**
    * Create an n x n matrix (where n is the number of strings)
    * containing the alignment between the i-th and the j-th string.
    * The matrix is symmetric, i.d. m(i, j) = m(j, i).
    */
  val alignments = {
    val n = strings.size
    val alignments = Array.ofDim[Alignment](n, n)
    for (i <- 0 until n; j <- i until n) {
      // Note that j starts with i (not i + 1)
      val s1 = strings(i)
      val s2 = strings(j)
      val a = new Alignment(s1, s2, mode)
      alignments(i)(j) = a
      alignments(j)(i) = a
    }
    alignments
  }

  /**
    * Here the MSA is calculated.
    */
  val multipleAlignment: Array[AlignedString]
}