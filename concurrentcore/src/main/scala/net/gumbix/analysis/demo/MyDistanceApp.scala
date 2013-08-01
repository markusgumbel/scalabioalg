package net.gumbix.analysis.demo
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

   This script is based on the net.gumbix.string.alignment.EditDistanceApp.scala
*/

import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import swing._
import javax.swing.UIManager
import java.awt.{Dimension, Color, Font}
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import scala.swing.event.ButtonClicked
import net.gumbix.dynpro.concurrency.ConClass._
import scala.swing.event.ButtonClicked
import net.gumbix.dynpro.concurrency.ConMode._
import scala.swing.event.ButtonClicked

/**
 * Demo for the edit distance. Was used for a open campus demo.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object MyDistanceApp {
  val text = new TextArea {
    text = "empty"
    editable = false
    font = Font.decode(Font.MONOSPACED + "-18")
    background = new Color(240, 240, 240)
  }

  val items = "wiesengrund lampe bringen" :: "schweinehund ampel trinken" :: Nil

  def main(args: Array[String]) {

    // Install the look and feel
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case e: Exception =>
    }

    val top = new MainFrame {
      preferredSize = new Dimension(700, 500)
      val textFrom = new TextField {
        text = "wiesengrund lampe bringen Meier Messer Ziele Straßburg"
        font = Font.decode(Font.MONOSPACED + "-18")
      }
      val textTo = new TextField {
        text = "schweinehund ampel trinken Maier Metzger Zeile Strassbourg"
        font = Font.decode(Font.MONOSPACED + "-18")
      }
      title = "Edit Distance"
      contents = new BorderPanel {
        val editPart = new BorderPanel {
          val stringPart = new BorderPanel {
            val labelPart = new BorderPanel {
              val from = new Label {
                text = "from:"
              }
              layout(from) = BorderPanel.Position.North
              val to = new Label {
                text = "to:"
              }
              layout(to) = BorderPanel.Position.South
            }
            layout(labelPart) = BorderPanel.Position.West
            val textfieldPart = new BorderPanel {
              layout(textFrom) = BorderPanel.Position.North
              layout(textTo) = BorderPanel.Position.South
            }
            layout(textfieldPart) = BorderPanel.Position.Center
            /*
            val menuPart = new BorderPanel {
              val menuFrom = new ComboBox(items) {
              }
              layout(menuFrom) = BorderPanel.Position.North
              val menuTo = new ComboBox(items) {
                reactions += {
                  case ButtonClicked(menuTo) => textFrom.text = selection.item
                }
              }
              layout(menuTo) = BorderPanel.Position.South
            }
            layout(menuPart) = BorderPanel.Position.East
            */
          }
          layout(stringPart) = BorderPanel.Position.Center
          val paramPart = new BorderPanel {
            val calcButton = new Button {
              text = "Calc."
              reactions += {
                case ButtonClicked(calcButton) => {
                  calc(textFrom.text, textTo.text)
                }
              }
            }
            layout(calcButton) = BorderPanel.Position.Center
          }
          layout(paramPart) = BorderPanel.Position.East
        }
        layout(editPart) = BorderPanel.Position.North
        // Info part:
        val infoPart = new BorderPanel {
          val scrollpane = new ScrollPane {
            contents = text
          }
          layout(scrollpane) = BorderPanel.Position.Center
        }
        layout(infoPart) = BorderPanel.Position.Center
      }
    }

    top.pack()
    top.visible = true
    1
  }

  object dp extends Alignment(new StringBuilder(""), new StringBuilder(""), AlignmentMode.GLOBAL) {
    override val (config, values) = (setConfig(LEFT_UP, EVENT),
      Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1))
  }

  def calc(s1: String, s2: String) {
    dp.updateXY(s1, s2)
    val solution = dp.solution
    text.text = "Number of changes: " + -dp.similarity
    text.text += "\n\n" + dp.makeAlignmentString(solution)
    text.text += "\n\n" + dp.mkMatrixString(solution)
    text.text += "\n\n"
    solution.foreach(e => text.text += e.decision)
  }
}