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

import net.gumbix.bioinf.string.alignment.AlignmentMode.AlignmentMode

import scala.collection.mutable.ArrayBuffer

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class StarAlignment(strings: Array[String], ll: Boolean = false)
  extends MultipleAlignment(strings)
    with ProgressiveAlignment {

  logLevel = ll

  /**
    * Calculate the multiple alignment with the star algorithm.
    */
  val multipleAlignment = {

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
      logln("list of similarities: " + similarities.toList.mkString(", "))
      val max = similarities reduceLeft (_ max _)

      val rootIdx = similarities.indexOf(max)
      logln("root " + alignments(rootIdx)(rootIdx).s1 +
      " with max. " + max)
      rootIdx
    }

    val treeBuffer = new ArrayBuffer[AlignedString]
    // Create a list of all indices except the root index:
    val idxs = (0 until alignments.size).toArray.filter(_ != rootIdx)

    // Get the aligned strings for the root and the first child.
    // Note that first aligned string must be the root. If
    // rootIdx is greater than idx the aligned string have to be swapped.
    val (root, leaf) = if (rootIdx > idxs(0)) {
      alignments(rootIdx)(idxs(0)).alignedStrings.swap
    } else {
      alignments(rootIdx)(idxs(0)).alignedStrings
    }

    treeBuffer += root
    treeBuffer += leaf
    val alignedRoot = treeBuffer(0)

    logln("\nInitial pairwise Alignment:")
    logln(treeBuffer.mkString("\n"))

    // Go through the remaining children...
    for (idx <- idxs.drop(1)) {

      val (root, leaf) = if (rootIdx > idx) { // See comment above.
        alignments(rootIdx)(idx).alignedStrings.swap
      } else {
        alignments(rootIdx)(idx).alignedStrings
      }

      // logln("\ncompare root idx = " + rootIdx + " with leaf idx = " + idx)
      // logln("Current  root = " + treeBuffer(0).toString)
      logln("\nAlignment of root and new leaf:")
      logln("root: " + root.toString)
      logln("leaf: " + leaf.toString)

      // Identify inserts such that the current root and
      // the root aligned with idx are compatible:
      val (insertPos, insertPosMsa) = getInsertions(root, leaf)
      // Also consider the gaps in the aligned root node:
      val aligendRootGaps = alignedRoot.gaps()

      logln("Insert positions in existing msa: " +
        insertPosMsa.mkString(", "))
      logln("Insert positions in new leaf    : " +
        aligendRootGaps.mkString(", "))

      // Add the inserts to the idx-th string:
      // reversed because ... TODO
      insertGaps(leaf, aligendRootGaps.reverse)

      // Also insert the inserts in the current tree buffer:
      treeBuffer.foreach(insertGaps(_, insertPosMsa))

      treeBuffer += leaf // Finally, add the new string.

      logln("\nMultiple Alignment:")
      logln(treeBuffer.mkString("\n"))
    }
    logln("\nDone.\n")

    treeBuffer.toArray
  }
}