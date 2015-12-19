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

import collection.mutable.ArrayBuffer
import net.gumbix.bioinf.string.alignment.GapType._
import net.gumbix.bioinf.string.alignment.AlignmentMode._
import org.apache.commons.math.stat.Frequency
import net.gumbix.util.Logger
import net.gumbix.dynpro.DynProMatrixPrinter

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class MultipleAlignment(val strings: Array[String], val mode: AlignmentMode)
        extends MultipleAlignmentPrinter with DynProMatrixPrinter[Int]
                with Score with Logger {
  logLevel = true
  formatter = INT

  /**
   * Calculate the consensus sequence.
   */
  def consensus = {
    val m = multipleAlignment(0).size
    val c = for (j <- 0 until m) yield {
      val histo = new Frequency
      // TODO quite ugly
      val symbols = multipleAlignment.map(s => s(j)).
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

  /**
   * @return The number of differences column-wise between the
   * consensus string the and the aligned strings. 
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
   * using the sum of pairs method.
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
  protected val alignments = {
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

  def matrix: Array[Array[Option[Double]]] = {
    val m = Array.ofDim[Option[Double]](alignments.size, alignments.size)
    for (i <- 0 until alignments.size; j <- 0 until alignments.size) {
      m(i)(j) = Some(alignments(i)(j).similarity)
    }
    m
  }

  /**
   * TODO what is the tree?
   */
  protected val multipleAlignment = {

    /*
    * Calculate root index. Add the similarities for each row, i.e.
    * iterate over the columns per row. Identify the row-index with
    * the highest sum of similarities.
    * Let M be the n x n matrix of similarities and e=(1, 1, ..., 1)
    * a vector. Then root index is max(M e).
    */
    val rootIdx = {
      // similarities is an array of sum of column-similarities
      val similarities = alignments.map {
        ai => // this is row i
        // Sum over all columns for row ai:
          ai.map(aij => aij.similarity).reduceLeft(_ + _)
      }
      logln("alignments:")
      logln(mkMatrixString)
      logln("list of similarities = " + similarities.toList.mkString(", "))
      val max = similarities reduceLeft (_ max _)

      logln("max = " + max)
      similarities.indexOf(max)
    }

    /**
     * Insert gaps. Start from the end of the sorted list
     * of positions as this avoids a recalculating of
     * the shifted indices.
     */
    def insertGaps(s: AlignedString, gaps: List[Int]) {
      gaps.reverse.foreach(s.insertGapBefore(_, GAP))
    }

    /**
     * Calculate the positions in the strings s1 and s2 where a
     * gap needs to be inserted (before) to make the two strings
     * compatible. The principle is: once a gap, always a gap.
     * @param s1 First aligned string
     * @param s2 Second aligned string
     * @return a tuple of two lists containing the insert-positions
     * for string s1 and s2.
     */
    def getInsertions(s1: AlignedString, s2: AlignedString):
    Tuple2[List[Int], List[Int]] = {
      var i = 0
      var j = 0
      var e1: List[Int] = Nil
      var e2: List[Int] = Nil

      // Read a char c1 and c2 from the strings s1 and s2.
      // Move the pointer for c1 and c2 forward depending
      // on the read characters.
      while (i < s1.size && j < s2.size) {
        if ((s1.isGapAt(i) && s2.isGapAt(j)) || s1(i) == s2(j)) {
          // Case 1: Both characters are equal.
          // Do nothing but move both pointers:
          i += 1
          j += 1
        } else if (s1.isGapAt(i)) {
          // Case 2: c1 is a gap. A gap must be
          // inserted on s2 at position j.
          // Move only pointer c1 and leave c2 where it is.
          e2 = j :: e2
          i += 1
        } else {
          // Case 3: c2 has a gap. A gap must be
          // inserted on s1 at position i.
          // Move only pointer c2 and leave c1 where it is.
          e1 = i :: e1
          j += 1
        }
      }
      // There might be unread characters on either s1 or s2.
      (i until s1.size).foreach {
        _ => e2 = j :: e2
      }
      (j until s2.size).foreach {
        _ => e1 = i :: e1
      }
      (e1.reverse, e2.reverse)
    }

    val treeBuffer = new ArrayBuffer[AlignedString]
    // Create a list of all indices except the root index:
    val idxs = (0 until alignments.size).toArray.filter(_ != rootIdx)

    // Get the aligned strings for the root and the first child.
    // Note that first aligned string must be the root. If
    // rootIdx is greater than idx the aligned string have to be swapped.
    val pair = if (rootIdx > idxs(0)) {
      alignments(rootIdx)(idxs(0)).alignedStrings.swap
    } else {
      alignments(rootIdx)(idxs(0)).alignedStrings
    }

    treeBuffer += pair._1
    treeBuffer += pair._2

    // Go through the remaining children...
    for (idx <- idxs.drop(1)) {

      logln("\nMultiple Alignment (before step):")
      logln(treeBuffer.mkString("\n"))

      val aPair = if (rootIdx > idx) { // See comment above.
        alignments(rootIdx)(idx).alignedStrings.swap
      } else {
        alignments(rootIdx)(idx).alignedStrings
      }

      logln("\ncompare root idx = " + rootIdx + " with leaf idx = " + idx)
      logln("Current  root = " + treeBuffer(0).toString)
      logln("Alignment of root with new leaf:")
      logln("root =          " + aPair._1.toString)
      logln("leaf  =         " + aPair._2.toString)

      // Identify inserts such that the current root and
      // the root aligned with idx are compatible:
      val insertPos = getInsertions(treeBuffer(0), aPair._1)

      logln("Inserts = " + insertPos.toString)

      // Insert the inserts in the current tree buffer:
      treeBuffer.foreach(insertGaps(_, insertPos._1))
      // Also add the inserts to the idx-th string:
      insertGaps(aPair._2, insertPos._2)
      treeBuffer += aPair._2

      logln("\nMultiple Alignment (after step):")
      logln(treeBuffer.mkString("\n"))
    }
    logln("\nDone.\n")

    treeBuffer.toArray
  }
}