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
package net.gumbix.layout.test

import net.gumbix.layout.Element
import net.gumbix.layout.Element._
import org.junit.Test
import org.junit.Assert._

import scala.math._

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class LayoutTest {

  @Test
  def test01 {
    align = RIGHT
    val x1 = line("Hello") above line("World!")
    assertEquals(" Hello\nWorld!", x1.toString)
    val x2 = line("What?") beside x1
    assertEquals("What? Hello\n     World!", x2.toString)
  }

  @Test
  def test01b {
    align = {s: String => s.length}
    val x1 = line("Hallo") above line("Welt")
    println(x1)
    val x2 = line("Und?") above line("Ha!")
    val x3 = x1 beside x2
    println(x3)
  }

  @Test
  def test01c {
    align = {s: String => min(max(s.indexOf('.') + 1, 0), s.length)}
    val x1 = line("12.34") above line("1.0")

    println(expandableLine("|", '|') beside x1 beside expandableLine("|", '|'))
  }

  @Test
  def test02 {
    align = NUMBER

    val m: Array[Array[Double]] = Array(
      Array(12.34, 2.0),
      Array(1.0, 2.123456)
      )
    println(makeTable(m))
  }

  @Test
  def test03 {
    align = CENTER
    val m: Array[Array[String]] = Array(
      Array("Das ist", "ein Test"),
      Array("Was", "steht denn hier?"),
      Array("Das ist", "ein Test"),
      Array("Was", "steht denn hier?")
      )
    println(makeTable(m, Array("1", "2", "3", "4"), Array("a", "b")))
  }

  @Test
  def test04 {
    align = NUMBER
    val matrix: Array[Array[Double]] = Array.ofDim(20, 10)
    for (i <- 0 until matrix.size; j <- 0 until matrix(0).size) {
      matrix(i)(j) = random * random * 100 - 20
    }
    println(makeTable(matrix))
  }


  @Test
  def test05 {
    val s = line(".......ACAGGCT") above line("11111112|22|||") above line("ACTTTATGCCTGCT")
    println(s)
  }

  def makeTable[A](matrix: Array[Array[A]]): Element = {
    val rowHeaders = (1 to matrix.size).map(x => x.toString).toArray
    val colHeaders = (1 to matrix(0).size).map(x => x.toString).toArray
    makeTable(matrix, rowHeaders, colHeaders)
  }

  def makeTable[A](matrix: Array[Array[A]],
                   rowHeader: Array[String],
                   colHeader: Array[String]): Element = {
    var firstColumn = line("") above expandableLine("-", '-')
    rowHeader.foreach(r => firstColumn = firstColumn above line(r))

    var table = expandableLine("|", '|') beside firstColumn beside expandableLine("|", '|')
    for (j <- 0 until matrix(0).size) {
      var col = line(colHeader(j))
      col = col above expandableLine("-", '-')
      for (i <- 0 until matrix.size) {
        col = col above line(matrix(i)(j).toString)
      }
      table = table beside col beside expandableLine("|", '|')
    }
    table
  }
}