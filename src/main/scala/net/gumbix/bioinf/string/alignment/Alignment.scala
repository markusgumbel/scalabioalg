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

import net.gumbix.bioinf.string.alignment.AlignmentMode._
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.dynpro.CellPosition._
import net.gumbix.dynpro.Backpropagation
import net.gumbix.bioinf.string.alignment.GapType._
import net.gumbix.dynpro.{PathEntry, MatrixPrinter, Idx, DynPro}

/**
 * s1 is supposed to be the search string whereas s2 is the (longer) original
 * string. So it should be |s1| < |s2|.
 * This algorithm uses the "matrixForwardPathBackward" as this is the
 * common method in bioinformatics.
 * @param s1 The search string
 * @param s2 The original string
 * @param mode What kind of alignment is used
 * @param substMatrix A substitution matrix for matches and mismatches.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class Alignment(val s1: String, val s2: String,
                val mode: AlignmentMode,
                override val substMatrix: Option[String])
        extends DynPro[AlignmentStep]
                with Backpropagation[AlignmentStep]
                with MatrixPrinter[AlignmentStep]
                with AlignmentPrinter[AlignmentStep]
                with Score {
  // Helper constructor if no substitution matrix is used:
  def this(s1: String, s2: String, mode: AlignmentMode) = this (s1, s2, mode, None)

  formatter = INT

  def n = s1.length + 1

  def m = s2.length + 1

  override val backpropagationStart = mode match {
    case LOCAL_LEFT_ALIGNMENT => MAXIMUM_VALUE_LAST_ROW
    case LOCAL_RIGHT_ALIGNMENT => MAXIMUM_INDICES
    case LOCAL_CENTERED => MAXIMUM_VALUE_LAST_ROW
    case LOCAL_OPTIMAL => MAXIMUM_VALUE
    case GLOBAL => MAXIMUM_INDICES
  }

  /**
   * The similarity of the alignment.
   */
  def similarity = {
    val idx = cellIndices(backpropagationStart)
    matrix(idx.i)(idx.j).get
  }

  /**
   * If we are at position (0, 0) there is no decision.
   * If we are at the first row (idx.i = 0) only INSERT is possible.
   * If we are at the left column (idx.j = 0) only DELETE is possible.
   * In any other case, the prev. state is BOTH.
   */
  def decisions(idx: Idx) = {
    if (idx.i == 0 && idx.j == 0) Array(NOTHING)
    else if (idx.i == 0) Array(INSERT)
    else if (idx.j == 0) Array(DELETE)
    else Array(INSERT, DELETE, BOTH)
  }

  def prevStates(idx: Idx, d: AlignmentStep) = d match {
    case NOTHING => Array()
    case _ => Array(idx + deltaIdx(d))
  }

  def value(idx: Idx, d: AlignmentStep) = d match {
    case NOTHING => 0
    case DELETE => {
      if (idx.j == 0 && allowLeftGapsString2) 0 else values(d)
    }
    case INSERT => {
      if (idx.i == 0 && allowLeftGapsString1) 0 else values(d)
    }
    case _ => score(s1(idx.i - 1), s2(idx.j - 1))
  }

  /**
   * If true gaps at the beginning of s1 are for free.
   * s2 can proceed without any costs => first row is 0.
   */
  val allowLeftGapsString1 = mode match {
    case LOCAL_LEFT_ALIGNMENT => false
    case LOCAL_RIGHT_ALIGNMENT => true
    case LOCAL_CENTERED => true
    case LOCAL_OPTIMAL => false
    case GLOBAL => false
  }

  /**
   * If true gaps at the beginning of s2 are for free.
   * s1 can proceed without any costs => first column is 0.
   */
  val allowLeftGapsString2 = mode match {
    case LOCAL_LEFT_ALIGNMENT => false
    case LOCAL_RIGHT_ALIGNMENT => false
    case LOCAL_CENTERED => false
    case LOCAL_OPTIMAL => false
    case GLOBAL => false
  }

  override def reviseMax(max: Double) = {
    if (mode == LOCAL_OPTIMAL) {
      if (max < 0) 0 else max
    } else {
      super.reviseMax(max)
    }
  }

  def alignedStrings(): Tuple2[AlignedString, AlignedString]
  = alignedStrings(solution)

  /**
   * @param solution A possible solution for the alignment.
   * @return The aligned string based on on the given solution.
   */
  def alignedStrings(solution2: List[PathEntry[AlignmentStep]]) = {

    val solution = solution2.toArray

    import Math._

    if (solution.isEmpty) {
      val as1 = new AlignedString(s1)
      val as2 = new AlignedString(s2)
      as1.insertGapBefore(0, s2.length, EMPTY)
      as2.insertGapAtEnd(s1.length, EMPTY)
      (as1, as2)
    }

    // Prefix calculation
    val ib = solution(0).currCell.i
    val jb = solution(0).currCell.j

    // Calc. the max. chars to print at the beginning:
    val b = max(ib, jb)
    // The difference is the prefix-gap for each string:
    val gapB1 = b - ib
    val gapB2 = b - jb

    // Suffix calculation
    val ih = solution(solution.size - 1).currCell.i
    val jh = solution(solution.size - 1).currCell.j
    // Calc. the max. chars to print at the end:
    val ie = n - 1 - ih
    val je = m - 1 - jh
    val e = max(ie, je)
    // The difference is the suffix-gap for each string:
    val gapE1 = e - ie
    val gapE2 = e - je

    val as1 = new AlignedString(s1)
    val as2 = new AlignedString(s2)

    as1.insertGapBefore(0, gapB1, EMPTY)
    as2.insertGapBefore(0, gapB2, EMPTY)

    var i1 = gapB1
    var i2 = gapB2
    for (i <- 1 until solution.size) {
      solution(i).decision match {
        case INSERT => as1.insertGapBefore(i1, 1, GAP)
        case DELETE => as2.insertGapBefore(i2, 1, GAP)
        case _ =>
      }
      i1 += 1
      i2 += 1
    }

    as1.insertGapAtEnd(gapE1, EMPTY)
    as2.insertGapAtEnd(gapE2, EMPTY)

    (as1, as2)
  }

  /**
   * Decision -> Previous state. What are the previous states if the decision is given?
   */
  val deltaIdx = Map(INSERT -> Idx(0, -1), DELETE -> Idx(-1, 0), BOTH -> Idx(-1, -1))

  override def rowLabels = makeLabels(s1).toArray

  override def columnLabels = Some(makeLabels(s2).toArray)

  private def makeLabels(s: String) = "-" :: s.toCharArray.map(_.toString).toList
}