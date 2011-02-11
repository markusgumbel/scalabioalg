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
package net.gumbix.bioinf.struct

import net.gumbix.dynpro.Idx
import net.gumbix.bioinf.struct.NussinovState._
import edu.uci.ics.jung.visualization.VisualizationViewer
import collection.mutable.{HashMap}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import org.apache.commons.collections15.Transformer
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, ModalGraphMouse}
import java.awt._
import event.{ActionEvent, ActionListener}
import javax.swing.{JTextField, JFrame}
import edu.uci.ics.jung.graph.{Graph, SparseMultigraph}

/**
 * A viewer for the 2D structure of RNA.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object RNAStruct2DViewer {
  def main(args: Array[String]) {
    val s = "UCCCUGGUGGUCUAGUGGDUAGGAUUCGGCGCUCUCACCGCCGCGGCCCGGGUUCGAUUCCCGGUCAGGGAACCA"
    val vv = graphPanel(graph(s))
    val jf = new JFrame("RNA 2D Viewer")
    jf.getContentPane.setLayout(new BorderLayout())
    val textfield = new JTextField(s)
    textfield.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        jf.getContentPane.remove(1)
        val vv = graphPanel(graph(textfield.getText))
        jf.getContentPane.add(vv, BorderLayout.CENTER)
        jf.pack
      }
    })
    jf.getContentPane.add(textfield, BorderLayout.NORTH)
    jf.getContentPane.add(vv, BorderLayout.CENTER)
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.pack
    jf.setVisible(true)
  }

  def graphPanel(graph: Graph[Node[Char], Edge]) = {
    val layout = new KKLayout(graph)
    layout.setAdjustForGravity(true)
    layout.setExchangeVertices(false)
    //val layout = new ISOMLayout(graph)
    val vv = new VisualizationViewer[Node[Char], Edge](layout)

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

  object vertexPaint extends Transformer[Node[Char], Paint] {
    def transform(n: Node[Char]) = n.node match {
      case 'A' => Color.GREEN.darker
      case 'U' => Color.GREEN.brighter
      case 'C' => Color.RED.darker
      case 'G' => Color.RED.brighter
      case _ => Color.LIGHT_GRAY
    }
  }

  object edgeStrokeTransformer extends Transformer[Edge, Stroke] {
    val linkStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
      BasicStroke.JOIN_MITER, 10.0f, Array(10.0f), 0.0f)

    val edgeStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
      BasicStroke.JOIN_MITER, 10.0f, Array(1.0f), 0.0f)

    def transform(s: Edge) = s match {
      case s: Link => linkStroke
      case _ => edgeStroke
    }
  }

  def graph(s: String) = {
    // val dp = new NussinovCount(s)
    val dp = new NussinovEnergy(s)
    val solution = dp.solution(Idx(0, dp.n - 1))
    println(dp.mkMatrixString(solution))

    val list = new HashMap[Int, Node[Char]]()
    var g = new SparseMultigraph[Node[Char], Edge]
    for (id <- 0 until s.length) {
      list(id) = new Node[Char](s.charAt(id), id)
      g.addVertex(list(id))
    }
    // Build the chain:
    for (id <- 0 until s.length - 1) {
      g.addEdge(new Edge(id), list(id), list(id + 1))
    }
    // Create the associations:
    solution.foreach {
      e =>
        val d = e.decision
        d.move match {
          case PAIR =>
            // TODO
            val id = d.idx.i * 100 + d.idx.j * 1000
            g.addEdge(new Link(id), list(d.idx.i), list(d.idx.j))
          case _ =>
        }
    }
    g
  }
}

class Node[A](val node: A, id: Int) {
  override def toString = node.toString
}

class Edge(id: Int) {
  override def toString = (id+1).toString
}

class Link(id: Int) extends Edge(id) {
  override def toString = ""
}