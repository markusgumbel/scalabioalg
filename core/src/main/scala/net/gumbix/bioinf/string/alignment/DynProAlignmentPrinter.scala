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

import net.gumbix.dynpro.PathEntry

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
trait DynProAlignmentPrinter[Decision] extends AlignmentPrinter {

  def alignedStrings(solution: List[PathEntry[Decision]]):
  Tuple2[AlignedString, AlignedString]

  /**
   * Creates an alignment.
   * @param solution A solution for the string alignment.
   * @return Two strings and a line inbetween.
   */
  def makeAlignmentString(solution: List[PathEntry[Decision]]): String = {
    val (als1, als2) = alignedStrings(solution)
    makeAlignmentString(als1, als2)
  }

  def makeLaTeXAlignmentString(solution: List[PathEntry[Decision]]): String = {
    val (als1, als2) = alignedStrings(solution)
    makeLaTeXAlignmentString(als1, als2)
  }
}