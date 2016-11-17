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
package net.gumbix.util.test

import net.gumbix.util.MatrixPrinter
import org.junit.Test
import org.junit.Assert._

/**
  * Test cases for aligned strings.
  * TODO Write more!
  *
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class MatrixPrinterTest {

  @Test
  def testMatrix() {
  }

  val matrixPrinter = new MatrixPrinter() {
    formatter = INT
    def matrix: Array[Array[Option[Double]]] = {
      Array.tabulate(5, 4) {
        (i, j) =>
          Some((i * j).toDouble)
      }
    }
  }
}