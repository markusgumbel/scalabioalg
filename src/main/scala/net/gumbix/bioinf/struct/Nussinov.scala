package net.gumbix.bioinf.struct

import net.gumbix.dynpro.{Idx, MatrixPrinter, DynPro}
import net.gumbix.bioinf.struct.NussinovState._

/**
 * Calculates the structure of an RNA molecule using the Nussinov
 * algorithm.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
abstract class AbstractNussinov(val s: String)
        extends DynPro[NussinovDecision]
                with MatrixPrinter[NussinovDecision] {
  formatter = INT

  def n = s.length

  def m = n

  /**
   * The size of hair pin loop. Default is 3.
   */
  def hairPinLoopSize = 2

  def decisions(idx: Idx) = {
    if (idx.j - idx.i < 1) Array()
    else {
      val splitList = if (idx.j - idx.i > 1) {
        val h = for (k <- (idx.i + 1) to (idx.j - 1)) yield {
          new NussinovDecision(SPLIT, idx, k, ' ', ' ')
        }
        h.toList
      } else Nil
      // val splitList = Nil
      val otherList = for (state <- NussinovState;
                           if (state != SPLIT)) yield {
        new NussinovDecision(state, idx, 0, s(idx.i), s(idx.j))
      }
      val u = splitList.toList ::: otherList.toList ::: Nil
      u.toArray
    }
  }

  def prevStates(idx: Idx, d: NussinovDecision) = d.move match {
    case EXTEND_BEGINNING => Array(Idx(idx.i + 1, idx.j))
    case EXTEND_END => Array(Idx(idx.i, idx.j - 1))
    case PAIR => Array(Idx(idx.i + 1, idx.j - 1))
    case SPLIT => {
      // SPLIT means k must be set:
      Array(Idx(idx.i, d.k), Idx(d.k + 1, idx.j))
    }
  }

  override def cellsSize = n * (n - 1) / 2

  /**
   * Start from position (0, 1) and go diagonally throw the matrix,
   * i.e. (1,2), ..., (n-1,n), (0,2), (1,3), ... until (n,n) is
   * reached.
   */
  val indices = {
    val h = for (l <- 1 until n; i <- 1 to n - l) yield {
      val j = i + l
      Idx(i - 1, j - 1)
    }
    h.toArray
  }  

  /**
   * Look-up function as long as I do not have
   * a better idea (aem mapping function).
   */
  override def getCellIndex(k: Int) = indices(k)

  override def rowLabels: Array[String] = s.toArray.map(_.toString)

  override def columnLabels = Some(rowLabels)
}

/**
 * This version maximizes the number of pairs.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class NussinovCount(s: String) extends AbstractNussinov(s) {
  def value(idx: Idx, d: NussinovDecision) = d.move match {
    case PAIR =>
      val validPair = (s(idx.i), s(idx.j)) match {
        case ('A', 'U') => true
        case ('U', 'A') => true
        case ('C', 'G') => true
        case ('G', 'C') => true
        case ('G', 'U') => true
        case ('U', 'G') => true
        case _ => false
      }
      if (validPair && (idx.j - idx.i > hairPinLoopSize)) 1 else 0
    case _ => 0
  }
}

/**
 * This version minimizes the free energy.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
class NussinovEnergy(s: String) extends AbstractNussinov(s) {


  override def extremeFunction = MIN
  
  def value(idx: Idx, d: NussinovDecision) = d.move match {
    // Values taken from Merkl p. 403:
    case PAIR =>
      val e = (s(idx.i), s(idx.j)) match {
        case ('A', 'U') => -2.0
        case ('U', 'A') => -2.0
        case ('C', 'G') => -3.0
        case ('G', 'C') => -3.0
        case ('G', 'U') => -1.0
        case ('U', 'G') => -1.0
        case _ => 0.0
      }
      if (idx.j - idx.i > hairPinLoopSize) e else 0
    case _ => 0
  }
}

case class NussinovDecision(move: NussinovState, idx: Idx, k: Int,
                            s1: Char, s2: Char) {
  override def toString = move match {
    case EXTEND_BEGINNING => "first(" + idx.i + ")=" + s1
    case EXTEND_END => "last(" + idx.j + ")=" + s2
    case PAIR => "pair" + idx.toString + "=" + s1 + "-" + s2
    case SPLIT => "split(" + k + ", " + (k + 1).toString + ")"
  }
}