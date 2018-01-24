package net.gumbix.bioinf.phylo.ui

import java.awt.{Color, Dimension, Graphics}
import java.text.DecimalFormat
import javax.swing.JPanel

import net.gumbix.bioinf.phylo.{JoinedTaxon, PhyloTree}

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class PhyloTreePanel2 extends JPanel {
  var tree: PhyloTree = null

  val formatter = new DecimalFormat("#0.00")

  setPreferredSize(new Dimension(500, 500))

  def setTree(tree: PhyloTree) {
    this.tree = tree
    updateUI()
    repaint()
  }

  override def paint(g: Graphics) {
    val d = getSize()
    val scale = d.height / 20.0
    var pos = (5.0, 0.5 * d.height / scale)
    var centerPos = pos
    var angle = 0.0 * 2 * Math.PI

    def drawEdge(length: Double) {
      val dx = Math.cos(angle) * length
      val dy = Math.sin(angle) * length

      val x2 = pos._1 + dx
      val y2 = pos._2 + dy

      val xp1 = (pos._1 * scale).asInstanceOf[Int]
      val yp1 = (pos._2 * scale).asInstanceOf[Int]
      val xp2 = (x2 * scale).asInstanceOf[Int]
      val yp2 = (y2 * scale).asInstanceOf[Int]
      val dxp = (dx * scale).asInstanceOf[Int]
      val dyp = (dy * scale).asInstanceOf[Int]

      g.setColor(Color.BLACK)
      g.drawLine(xp1, yp1, xp2, yp2)
      g.setColor(Color.GRAY)
      g.drawString(formatter.format(length) + "", xp1 + dxp / 2, yp1 + dyp / 2)
      pos = (x2, y2)
    }

    def drawNode(s: String) {
      val xp1 = (pos._1 * scale).asInstanceOf[Int]
      val yp1 = (pos._2 * scale).asInstanceOf[Int]
      g.setColor(Color.BLUE)
      g.drawString(s, xp1, yp1)
    }

    def rotate(a: Double) {
      val b = a * 2 * Math.PI / 360.0
      angle += b
    }

    def store() {
      centerPos = pos
    }

    def restore() {
      pos = centerPos
    }

    if (tree.allEdges.size > 2) {
      val allMapEdges = tree.allEdges.toMap
      for (j <- tree.allJoins) {
        val jt = j.asInstanceOf[JoinedTaxon]
        val t1 = jt.taxa(0)
        val t2 = jt.taxa(1)
        val w1 = allMapEdges(t1)
        val w2 = allMapEdges(t2)
        drawNode(t1.name)
        drawEdge(w1)
        rotate(60)
        store()
        drawEdge(w2)
        drawNode(t2.name)
        restore()
        rotate(-110) // -120
      }
      // One remaining node:
      val tf = tree.allEdges(tree.allEdges.size - 1)._1
      val wf = allMapEdges(tf)
      drawEdge(wf)
      drawNode(tf.name)
    }
  }

}
