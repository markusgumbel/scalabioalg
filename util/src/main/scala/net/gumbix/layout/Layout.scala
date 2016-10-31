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
package net.gumbix.layout

import net.gumbix.layout.Element._
import scala.math._

/**
 * A library for creating tables and other things that can be layouted. Adapted
 * from Odersky (p. ?).
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

abstract class Element {
  def contents: Array[Content]

  def height = contents.length

  def width: Int = if (height == 0) 0 else contents(0).value.length

  /**
   * The alignment of the content. Default is left.
   * Range is 1 (left-aligned) to width (right-aligned).
   */
  var alignment: Int = 0

  /**
   * Place this element above another (that).
   * Both elements are expanded such that both have the same
   * column size.
   */
  def above(that: Element): Element = {
    val this1 = this.widen(that.width, that.alignment)
    val that1 = that.widen(this.width, this.alignment)
    column(this1.contents ++ that1.contents, this1.alignment)
  }

  /**
   * Merge this column (element) with another. If the columns
   * have a different height, empty elements are added to fill
   * the gap.
   */
  def beside(that: Element) = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height

    val content = for ((line1, line2) <- this1.contents zip that1.contents)
    yield Content(line1.value + line2.value, line1.filler)
    column(content)
  }

  /**
   * Create a new element which fits the properties of the column.
   * @param w The width of the other element.
   * @param a The alignment of the other element.
   */
  def widen(w: Int, a: Int) = {
    val sLeft = max(a - alignment, 0)
    val sRight = max((w - a) - (width - alignment), 0)
    if (sLeft == 0 && sRight == 0) this
    else {

      val lines = for (line <- contents) yield {
        val fillString = line.filler.toString
        Content(fillString * sLeft + line.value + fillString * sRight, line.filler)
      }
      column(lines, alignment + sLeft)
    }
  }

  def heighten(h: Int): Element = {
    if (h <= height) this
    else {
      val filler = if (contents.size == 1) contents(0).filler else ' '
      val top = uniform(filler, width, (h - height) / 2)
      top.alignment = alignment
      val bot = uniform(filler, width, h - height - top.height)
      bot.alignment = alignment
      val res = top above this above bot
      res.alignment = alignment
      res
    }
  }

  override def toString = contents mkString "\n"
}

object Element {

  /**
   * A class that has a value and a fill-string if it is expanded.
   */
  case class Content(val value: String, val filler: Char) {
    override def toString = value
  }


  /**
   * A column.
   */
  class ArrayElement(val contents: Array[Content], align: Int)
          extends Element {
    alignment = align
  }

  /**
   * A single line
   */
  class LineElement(s: String, align: Int, filler: Char)
          extends Element {
    alignment = align

    val contents = Array(Content(s, filler))

    override def width = s.length

    override def height = 1
  }

  /**
   * A block of uniform elements.
   */
  class UniformElement(val get: Char, override val width: Int,
                       override val height: Int) extends Element {
    private val line = get.toString * width

    def contents = Array.tabulate(height){i => Content(line, ' ')}
  }

  val NUMBER = {s: String =>
    val i = s.indexOf('.')
    if (i == -1) s.length
    else i
  }
  val LEFT = {s: String => 0}
  val RIGHT = {s: String => s.length}
  val CENTER = {s: String => s.length / 2}

  var align: (String => Int) = CENTER

  def column(contents: Array[Content]): Element =
    new ArrayElement(contents, align(contents(0).value))

  def column(contents: Array[Content], align: Int): Element = new ArrayElement(contents, align)

  def uniform(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def line(line: String): Element = new LineElement(line, align(line), ' ')

  def line(line: String, align: Int): Element = new LineElement(line, align, ' ')

  def expandableLine(line: String, filler: Char): Element =
    new LineElement(line, align(line), filler)
}