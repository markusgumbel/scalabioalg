package net.gumbix.bioinf.phylo

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import net.gumbix.bioinf.phylo.ui.{PhyloTreePanel, PhyloTreePanel2}

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
object PhyloTreeViewer {
  def main(args: Array[String]) {

    def FMMethod(e: (Array[String], Array[Array[Double]])) = {
      val (taxa, dist) = e
      val metric = new FitchMargoliashMetric(taxa, dist)
      new FitchMargoliashTree(metric)
    }

    def NJMethod(e: (Array[String], Array[Array[Double]])) = {
      val (taxa, dist) = e
      val metric = new NeighborJoiningMetric(taxa, dist)
      new NeighborJoiningTree(metric)
    }

    var method: (((Array[String], Array[Array[Double]])) => PhyloTree) = NJMethod
    var selectedMetric = metrics.values.head

    val phyloPanel = new PhyloTreePanel()

    def refreshTree() {
      phyloPanel.setTree(method(selectedMetric._1, selectedMetric._2))
    }

    refreshTree()
    val jf = new JFrame("Phylogenetic Tree Viewer")

    val menuBar = new JMenuBar()
    val menu = new JMenu("Trees")
    menuBar add menu
    metrics.foreach {
      e =>
        val (label, metric) = e
        val menuItem = new JMenuItem(label)
        menuItem.addActionListener(new ActionListener() {
          override def actionPerformed(actionEvent: ActionEvent) {
            selectedMetric = metric
            refreshTree()
          }
        })
        menu.add(menuItem)
    }
    val algMenu = new JMenu("Algorithm")
    menuBar add algMenu
    val fmAlg = new JRadioButtonMenuItem("Fitch-Margoliash")
    fmAlg.setSelected(true)
    fmAlg.addActionListener(new ActionListener() {
      override def actionPerformed(actionEvent: ActionEvent) {
        method = FMMethod
        refreshTree()
      }
    })
    val njAlg = new JRadioButtonMenuItem("Neighbor Joining")
    njAlg.addActionListener(new ActionListener() {
      override def actionPerformed(actionEvent: ActionEvent) {
        method = NJMethod
        refreshTree()
      }
    })
    val group = new ButtonGroup()
    group.add(fmAlg)
    group.add(njAlg);
    algMenu.add(fmAlg)
    algMenu.add(njAlg)

    jf.setJMenuBar(menuBar)

    jf.getContentPane.setLayout(new BorderLayout())
    jf.getContentPane.add(phyloPanel, BorderLayout.CENTER)
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.pack
    jf.setVisible(true)
  }

  val metrics = {
    val A = Array

    Map(
      "Tree with 6 taxa" -> {
        (
          Array("A", "B", "C", "D", "E", "F"),
          A(
            A(0d, 3, 13, 12, 8, 9),
            A(0d, 0, 14, 13, 9, 10),
            A(0d, 0, 0, 9, 13, 14),
            A(0d, 0, 0, 0, 12, 13),
            A(0d, 0, 0, 0, 0, 5),
            A(0d, 0, 0, 0, 0, 0)
          )
          )
      },
      "Tree with 5 taxa" -> {
        (
          Array("A", "B", "C", "D", "E"),
          A(
            A(0d, 22, 39, 39, 41),
            A(0d, 0, 41, 41, 43),
            A(0d, 0, 0, 18, 20),
            A(0d, 0, 0, 0, 10),
            A(0d, 0, 0, 0, 0)
          )
          )
      },
      "Tree with 5 taxa (b)" -> {
        (
          Array("A", "B", "C", "D", "E"),
          A(
            A(0d, 3, 7, 8, 1),
            A(0d, 0, 6, 7, 4),
            A(0d, 0, 0, 3, 8),
            A(0d, 0, 0, 0, 9),
            A(0d, 0, 0, 0, 0)
          )
          )
      },
      "W15" -> {
        (
          Array("A", "B", "C", "D", "E"),
          A(
            A(0d, 4, 9, 9, 2),
            A(0d, 0, 6, 7, 4),
            A(0d, 0, 0, 3, 8),
            A(0d, 0, 0, 0, 9),
            A(0d, 0, 0, 0, 0)
          )
          )
      },
      "Tree with 4 taxa" -> {
        (
          Array("A", "B", "C", "D"),
          A(
            A(0d, 8, 12, 6),
            A(0d, 0, 18, 12),
            A(0d, 0, 0, 10),
            A(0d, 0, 0, 0)
          )
          )
      }
    )
  }
}