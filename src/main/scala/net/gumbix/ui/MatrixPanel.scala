package net.gumbix.ui

import swing._
import java.awt.Color._
import java.awt.{Dimension, Graphics2D, Color, Graphics}
import net.gumbix.dynpro.Idx

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

class MatrixPanel(rowLabels: Array[Char], columnLabels: Array[Char],
                  data: Array[Data]) extends Panel {
  preferredSize = new Dimension(500, 500)
  background = WHITE

  def minSize = Math.min(size.width, size.height)

  def deltaX = Math.min(minSize / (columnLabels.size + 2),
    minSize / (rowLabels.size + 2))

  def deltaY = deltaX

  def boxSize = {
    val tmpBoxSize = deltaX / 3
    if (tmpBoxSize > 20) 20
    else if (tmpBoxSize < 3) 3 else tmpBoxSize
  }

  override def paint(g: Graphics2D) {

    super.paint(g)
    g setColor LIGHT_GRAY
    val maxX = (columnLabels.size + 1) * deltaX + deltaX / 2
    val maxY = (rowLabels.size + 1) * deltaY + deltaY / 2
    // Draw horizontal lines
    for (i <- 0 to rowLabels.size) {
      val y = (i + 1) * deltaY + deltaY / 2
      g.drawLine(0, y, maxX, y)
    }
    // Draw vertical lines
    for (j <- 0 to columnLabels.size) {
      val x = (j + 1) * deltaX + deltaX / 2
      g.drawLine(x, 0, x, maxY)
    }

    g setColor BLACK
    // Draw column labels
    val y = deltaY
    for (j <- 0 until columnLabels.size) {
      val x = (j + 2) * deltaX
      g.drawString(columnLabels(j).toString, x, y)
    }

    // Draw row labels
    val x = deltaX
    for (i <- 0 until rowLabels.size) {
      val y = (i + 2) * deltaY
      g.drawString(rowLabels(i).toString, x, y)
    }

    // Draw the dots
    g setColor BLUE
    for (dots <- data) {
      val d = dots.text
      val x = (dots.idx.j + 1) * deltaX + deltaX
      val y = (dots.idx.i + 1) * deltaY + deltaY
      if (d == "") {
        g.fillRect(x - boxSize, y - boxSize, 2 * boxSize, 2 * boxSize)
      } else {
        g.drawString(d, x + boxSize + 5, y + boxSize)
      }
    }
  }
}

case class Data(idx: Idx, text: String) 