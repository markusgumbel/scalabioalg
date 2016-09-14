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
package net.gumbix.bioinf.string.alignment.test

import junit.framework.TestCase
import junit.framework.Assert._
import net.gumbix.bioinf.string.alignment.{AlignmentPrinter, AlignedString}
import net.gumbix.bioinf.string.alignment.GapType._

/**
 * Test cases for aligned strings.
 * TODO Write more!
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class AlignedStringTest extends TestCase {
  def testAlignedString() {

    val als = new AlignedString("hello")
    assertTrue(als.size == 5)

    als.insertGapBefore(0, 5, GAP)
    assertTrue(als.size == 10)
    assertEquals(als.toString(), "-----hello")

    als.insertGapBefore(6, 1, GAP)
    assertEquals(als.toString, "-----h-ello")

    als.insertGapBefore(als.size, 4, GAP)
    assertEquals(als.toString(), "-----h-ello----")
    assertEquals(als(5).toString, "h")
    assertEquals(als.primaryString, "hello")
  }

  def testParserNoGaps() {
    val s1 = new AlignedString("ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
  }

  def testParser1Gap() {
    val s1 = new AlignedString("AC-TGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(2), true)
  }

  def testParser2Gap() {
    val s1 = new AlignedString("AC--TGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(2), true)
    assertEquals(s1.isGapAt(3), true)
  }

  def testParserGapBeginning() {
    val s1 = new AlignedString("-ACTGAG")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.size, 7)
    assertEquals(s1.isGapAt(0), true)
    assertEquals(s1.isGapAt(6), false)
  }

  def testParserGapEnd() {
    val s1 = new AlignedString("ACTGAG-")
    assertEquals(s1.primaryString, "ACTGAG")
    assertEquals(s1.size, 7)
    assertEquals(s1.isGapAt(0), false)
    assertEquals(s1.isGapAt(6), true)
  }

  def testLaTeX() {
    val s1 = new AlignedString("-ACTGAG")
    val s2 = new AlignedString("GAAT--G")
    val latex = new AlignmentPrinter(){}.makeLaTeXAlignmentString(s1, s2)
    val should = "\\begin{tikzpicture}\n\\matrix (matrix) [matrix of nodes,ampersand replacement=\\&,alignment] {\n  - \\& A \\& C \\& T \\& G \\& A \\& G \\\\\n    \\& \\textcolor{gray}{$\\vert$} \\&   \\& \\textcolor{gray}{$\\vert$} \\&   \\&   \\& \\textcolor{gray}{$\\vert$} \\\\\n  G \\& A \\& A \\& T \\& - \\& - \\& G \\\\\n};\n\\node[left=2mm of matrix-1-1] {$s_1$};\n\\node[left=2mm of matrix-3-1] {$s_2$};\n\\end{tikzpicture}"
    assertEquals(latex, should)
  }
}