package net.gumbix.bioinf.phylo.ui

import java.awt._
import javax.swing.JPanel

import edu.uci.ics.jung.algorithms.layout.{KKLayout, DAGLayout, SpringLayout2, ISOMLayout}
import edu.uci.ics.jung.graph.{Graph, SparseMultigraph}
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, ModalGraphMouse}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import net.gumbix.bioinf.phylo.{PhyloTree, FitchMargoliashTree, JoinedTaxon, Taxon}
import org.apache.commons.collections15.Transformer

import scala.collection.mutable.HashMap

/**
  * A panel for a phylogenetic tree.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class PhyloTreePanel extends JPanel {

  /**
    * @param m The tree to be rendered.
    */
  def setTree(m: PhyloTree) {

    object vertexPaint extends Transformer[Node, Paint] {
      def transform(n: Node) = n.taxon match {
        case jt: JoinedTaxon => Color.LIGHT_GRAY
        case _ => Color.GREEN
      }
    }

    object edgeLengthTransformer extends Transformer[Edge, Integer] {
      def transform(e: Edge) = e.dist.asInstanceOf[Int] * 10
    }

    object edgeStrokeTransformer extends Transformer[Edge, Stroke] {
      val edgeStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER, 10.0f, Array(1.0f), 0.0f)

      def transform(s: Edge) = s match {
        case _ => edgeStroke
      }
    }

    def graphPanel(graph: Graph[Node, Edge]) = {
      val layout = new ISOMLayout(graph)
      // val layout = new SpringLayout2(graph, edgeLengthTransformer)
      // val layout = new DAGLayout(graph)

      // val layout = new KKLayout(graph)
      // layout.setAdjustForGravity(true)
      // layout.setExchangeVertices(false)
      val vv = new VisualizationViewer[Node, Edge](layout)

      vv.getRenderContext.setVertexFillPaintTransformer(vertexPaint)
      vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
      vv.getRenderContext.setEdgeLabelTransformer(new ToStringLabeller())
      vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)
      vv.getRenderContext.setEdgeStrokeTransformer(edgeStrokeTransformer)

      val gm = new DefaultModalGraphMouse()
      gm.setMode(ModalGraphMouse.Mode.TRANSFORMING)
      vv.setGraphMouse(gm)
      vv
    }

    /**
      * Create the graph with JUNG nodes.
      * @param m Tree.
      * @return
      */
    def graph(m: PhyloTree) = {
      val tree = m.tree()
      val taxon2Dist = tree.toMap

      val taxon2Node = new HashMap[Taxon, Node]()
      var g = new SparseMultigraph[Node, Edge]

      var i = 1
      for ((taxon, dist) <- tree) {
        taxon match {
          case jt: JoinedTaxon => {
            val taxon1 = jt.taxa(0)
            val taxon2 = jt.taxa(1)
            val innerNode = new InnerNode(jt)
            g.addVertex(innerNode)
            taxon2Node(jt) = innerNode
            g.addEdge(new Edge(taxon2Dist(taxon1)), innerNode, taxon2Node(taxon1))
            g.addEdge(new Edge(taxon2Dist(taxon2)), innerNode, taxon2Node(taxon2))
          }
          case _ => {
            val leaf = new Node(taxon)
            g.addVertex(leaf)
            taxon2Node(taxon) = leaf
          }
        }
      }
      // Link the last two leafs and the missing inner node:
      val lastTaxon1 = tree(tree.size - 1 - 2)
      val lastTaxon2 = tree(tree.size - 1 - 1)
      val lastInnerTaxon = tree(tree.size - 1)
      val lastNode3 = taxon2Node(lastInnerTaxon._1)

      val newNode = new InnerNode(new JoinedTaxon(Array()))
      g.addEdge(new Edge(lastTaxon1._2), newNode, taxon2Node(lastTaxon1._1))
      g.addEdge(new Edge(lastTaxon2._2), newNode, taxon2Node(lastTaxon2._1))
      g.addEdge(new Edge(lastInnerTaxon._2), newNode, lastNode3)
      g
    }

    setLayout(new BorderLayout())
    removeAll()
    add(graphPanel(graph(m)))
    updateUI()
  }
}

class Node(val taxon: Taxon) {
  override def toString = taxon.toString
}

class InnerNode(taxon: Taxon) extends Node(taxon) {
  override def toString = ""
}

class Edge(val dist: Double) {
  override def toString = dist.toString
}
