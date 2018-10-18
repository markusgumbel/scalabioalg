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

import net.gumbix.bioinf.string.alignment.AlignmentStep._

/**
 * 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait Score {

  val substMatrix: Option[String] = None
  
  /**
   * Decision -> Value. What is the value for the decision?
   */
  val values = Map(INSERT -> -2, DELETE -> -2, MATCH -> 1, SUBSTITUTION -> -1)

  def score(c1: Char, c2: Char) = substMatrix match {
    case None => {
      (c1, c2) match {
        case ('-', '-') => values(INSERT)  // values(MATCH) // or 0?
        case ('-', d2) => values(INSERT)
        case (d1, '-') => values(INSERT)
        case (d1, d2) if (d1 == d2) => values(MATCH)
        case _ => values(SUBSTITUTION)
      }
    }
    case _ => 0 // TODO Read from matrix
  }
}