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

/**
  * A string that can have gaps. The size of the string
  * is the number of characters plus the size of the gaps.
  * An index position always includes the gaps.
  * @param s The input string that is either a plain string
  *          or it can contains gaps identified by the - character.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class AlignedString(s: String) {

  import net.gumbix.bioinf.string.alignment.GapType._

  require(s.length > 0)

  val primaryString = s.filter(c => c != '-').toString

  /**
    * Each position (0 .. primaryString.size + 1) points to the
    * position in the primary string. Example for primaryString = "Markus":
    * Init settings:
    * M: mapper(0) = 0
    * a: mapper(1) = 1
    * r: mapper(2) = 2
    * k: mapper(3) = 3
    * u: mapper(4) = 4
    * s: mapper(5) = 5
    * €: mapper(6) = 6
    * 0123456
    * Aligned string Markus
    * With gaps:
    * M: mapper(0) = 2
    * a: mapper(1) = 3
    * r: mapper(2) = 5
    * k: mapper(3) = 6
    * u: mapper(4) = 7
    * s: mapper(5) = 8
    * €: mapper(6) = 10
    * 012345678910
    * Aligned string --Ma-rkus-
    * + 1 because of artificial end character.
    */
  private val mapper = {
    val a = new Array[Tuple2[Int, GapType]](primaryString.length + 1)
    for (i <- 0 until a.size) a(i) = (i, PAIR)
    a
  }

  def alignedString = toString()

  def size = mapper(mapper.size - 1)._1

  def apply(pos: Int): Char = {
    val rPos = getMapperIndex(pos)
    val (mPos, gapType) = mapper(rPos)
    if (pos == mPos) primaryString(rPos) else gapType.toString.apply(0)
  }

  /**
    * Is position pos a gap? This is not true if there is a pointer
    * in mapper which points to pos.
    */
  def isGapAt(pos: Int) = pos != mapper(getMapperIndex(pos))._1

  def gaps() = {
    val gaps = for (i <- 0 to size) yield {
      if (isGapAt(i)) i else -1
    }
    gaps.filter(e => e >= 0).toList
  }

  def insertGapBefore(pos: Int, gapType: GapType) {
    insertGapBefore(pos, 1, gapType)
  }

  /**
    * Note: pos = string size is possible. This inserts gaps
    * at the end of the string.
    * @param pos    Index position where to insert the gap. The
    *               position includes any gaps.
    * @param length Size of the gap.
    * @param gapType
    */
  def insertGapBefore(pos: Int, length: Int, gapType: GapType) {
    if (pos < 0 || pos > this.size) {
      val text = "pos = " + pos + " must be in 0 ... " + this.size
      throw new IndexOutOfBoundsException(text)
    }
    if (length < 0) {
      val text = "size = " + length + " must be >= 0."
      throw new IndexOutOfBoundsException(text)
    }
    val mIdx = getMapperIndex(pos)
    for (i <- mIdx until mapper.size) {
      val (pos, _) = mapper(i)
      mapper(i) = (pos + length, gapType)
    }
  }

  def insertGapAtEnd(gapType: GapType) {
    insertGapAtEnd(1, gapType)
  }

  def insertGapAtEnd(length: Int, gapType: GapType) {
    insertGapBefore(this.size, length, gapType)
  }

  override def toString() = {
    val list = for (i <- 0 until size) yield {
      this (i)
    }
    list.mkString
  }

  /**
    * @param pos A position in the aligned string.
    * @return Index in mapper that points to pos
    */
  private def getMapperIndex(pos: Int) = {
    var i = 0
    var (vPos, gapType) = mapper(i)
    while (vPos < pos) {
      i += 1
      vPos = mapper(i)._1
    }
    i
  }

  private def stripString() {
    val idx = (0 until s.size)
    val l = idx zip s
    val pos = l.filter(c => c._2 == '-').map(c => c._1)
    pos.foreach {
      pos => insertGapBefore(pos, GAP)
    }
  }

  stripString()
}


object GapType extends Enumeration {
  type GapType = Value

  val GAP = Value("-")

  val EMPTY = Value("~")

  val PAIR = Value("#")
}