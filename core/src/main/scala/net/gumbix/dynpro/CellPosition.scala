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
package net.gumbix.dynpro

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object CellPosition extends Enumeration {
  type CellPosition = Value

  val MAXIMUM_VALUE = Value("Max. global value")
  val MAXIMUM_VALUE_LAST_ROW = Value("Max. last row")
  val MAXIMUM_VALUE_LAST_COLUMN = Value("Max. last column")
  val MAXIMUM_INDICES = Value("Max. indices, i.e. (n, m)")
}