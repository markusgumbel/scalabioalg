package net.gumbix.string.alignment

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

import scala.swing._
import scala.swing.event.ButtonClicked
import BorderPanel.Position._
import Orientation.{Horizontal, Vertical}

import javax.swing.UIManager
import java.awt.{Dimension, Color, Font}

import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment, AlignmentStep}
import AlignmentStep._
import net.gumbix.dynpro.concurrency.{ConClass, ConMode}
import ConClass._
import ConMode._
import javax.swing.border.TitledBorder

/**
 * Demo for the edit distance. Was used for a open campus demo.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object MyDistanceApp extends SimpleSwingApplication{
  private val (w, h) = (1100, 700)

  private val splitPane = new SplitPane(Horizontal){
    continuousLayout = true
    oneTouchExpandable = true
    resizeWeight = 0.80
    border = new TitledBorder("Sequential")

    topComponent = new ScrollPane {
      contents = matrix
    }
    bottomComponent = new ScrollPane {
      contents = result
    }
  }

  class MyTextArea(text: String) extends TextArea(text + " ..."){
    editable = false
    font = Font.decode(Font.MONOSPACED + "-18")
    background = new Color(240, 240, 240)
  }
  private val matrix = new MyTextArea("MATRIX")
  private val result = new MyTextArea("RESULT")

  /********** MODULE - START **********/
  private val (map, _seq, _con, br) = (
    Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1),
    "SEQ", "CON", "\n"
  )

  private class ConAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL) {
    override val (config, values) = (setConfig(LEFT_UP, EVENT), map)
  }

  private class SeqAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL)

  private def calc(s1: String, s2: String, mode: String) {
    val dp = if(mode == _con) new ConAlign(s1, s2) else new SeqAlign(s1, s2)
    val solution = dp.solution

    val (sim, align, _matrix, decision) = (
      "Number of changes: " + -dp.similarity + br,
      dp.makeAlignmentString(solution) + br,
      br+br + dp.mkMatrixString(solution) + br,
      solution.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2)
    )

    matrix.text = "MATRIX:" + _matrix
    result.text = sim + align + decision

    print("\n") //for debug purposes
  }

  /********** MODULE - END **********/


  /********** VIEW - START **********/
  // Install the look and feel
  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  } catch {
    case e: Exception =>
  }

  val top = new MainFrame {
    preferredSize = new Dimension(w, h)
    val textFrom = new TextField {
      text = "wiesengrund lampe bringen Meier Messer Ziele Straßburg"
      font = Font.decode(Font.MONOSPACED + "-18")
    }
    val textTo = new TextField {
      text = "schweinehund ampel trinken Maier Metzger Zeile Strassbourg"
      font = Font.decode(Font.MONOSPACED + "-18")
    }
    title = "Edit Distance 2.0"

    contents = new BorderPanel {
      layout(new BorderPanel {
        layout(new BorderPanel{
          layout(new BorderPanel {
            layout(new Label("from:")) = North
            layout(new Label("to:")) = South
          }) = West

          layout(new BorderPanel {
            layout(textFrom) = North
            layout(textTo) = South
          }) = Center
        }) = Center

        class MyRadioButton(label: String, modeLabel: String) extends RadioButton(label){
          listenTo(this)
          reactions += {
            case e: ButtonClicked => splitPane.border = new TitledBorder(modeLabel)
          }
        }

        layout(new BorderPanel{ /***** THIS PART HAS BEEN REMODELED *****/
          val seq = new MyRadioButton(_seq, "Sequential mode")
          val (group, radios) = (
            new ButtonGroup,
            List(seq, new MyRadioButton(_con, "Concurrent mode"))
          )
          group.buttons ++= radios
          group.select(seq)

          layout(new BoxPanel(Horizontal){
            contents ++= radios
          }) = North

          layout(new Button("Calc.") {
            reactions += {
              case e: ButtonClicked => {
                calc(textFrom.text, textTo.text, group.selected.get.text)
              }
            }
          }) = Center
        }) = East
      }) = North

      // Info part:
      layout(new BoxPanel(Vertical){
        contents += Swing.VStrut(20)
        contents += splitPane
      }) = Center
    }
  }

  /********** VIEW - END **********/

}
