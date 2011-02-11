package net.gumbix.bioinf.string.alignment.test

import net.gumbix.bioinf.string.alignment.AlignmentMode._
import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.bioinf.string.dotplot.DotPlot2
import junit.framework.TestCase

/**
 * Unit test cases.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class AlignmentTest extends TestCase with AlignmentOutput with ExampleData {
  def testAllAlignments() {
    println("All alignments")
    for ((s1, s2, comment) <- strings.values
         if comment.startsWith("nach Hütt p. 148."); mode <- AlignmentMode.values) {
      doAligmentDP(mode, s1, s2, comment)
    }
  }
}