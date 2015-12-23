package net.gumbix.bioinf.phylo

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import edu.uci.ics.jung.algorithms.layout.ISOMLayout
import edu.uci.ics.jung.graph.{Graph, SparseMultigraph}
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, ModalGraphMouse}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import net.gumbix.bioinf.phylo.ui.PhyloTreePanel
import org.apache.commons.collections15.Transformer

import scala.collection.mutable.HashMap

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
object PhyloTreeViewer {
  def main(args: Array[String]) {
    val metric = metrics.values.head
    val m = new FitchMargoliashTree(metric)

    val vv = new PhyloTreePanel()
    vv.setTree(m)
    val jf = new JFrame("Phylogenetic Tree Viewer")

    val menuBar = new JMenuBar()
    val menu = new JMenu("Trees")
    menuBar add menu
    metrics.foreach {
      e =>
        val (label, metric) = e
        val menuItem = new JMenuItem(label)
        menuItem.addActionListener(new ActionListener() {
          override def actionPerformed(actionEvent: ActionEvent)  {
            vv.setTree(new FitchMargoliashTree(metric))
          }
        })
        menu.add(menuItem)
    }
    jf.setJMenuBar(menuBar);

    jf.getContentPane.setLayout(new BorderLayout())
    jf.getContentPane.add(vv, BorderLayout.CENTER)
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.pack
    jf.setVisible(true)
  }

  val metrics = {
    val A = Array

    Map("Tree with 6 taxa" -> {
      val d: Array[Array[Double]] = A(
        A(0d, 3, 13, 12, 8, 9),
        A(0d, 0, 14, 13, 9, 10),
        A(0d, 0, 0, 9, 13, 14),
        A(0d, 0, 0, 0, 12, 13),
        A(0d, 0, 0, 0, 0, 5),
        A(0d, 0, 0, 0, 0, 0)
      )
      new FitchMargoliashMetric(Array("A", "B", "C", "D", "E", "F"), d)
    },
      "Tree with 4 taxa" -> {
        val d: Array[Array[Double]] = A(
          A(0d, 8, 12, 6),
          A(0d, 0, 18, 12),
          A(0d, 0, 0, 10),
          A(0d, 0, 0, 0)
        )
        new FitchMargoliashMetric(Array("A", "B", "C", "D"), d)
      }
    )
  }
}