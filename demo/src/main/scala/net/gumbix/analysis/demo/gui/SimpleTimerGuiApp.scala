package net.gumbix.analysis.demo.gui

import scala.swing._
import BorderPanel.Position._
import Orientation._
import scala.swing.event._
import scala.util.Random
import scala.collection.mutable.StringBuilder

import net.gumbix.analysis.demo.gui.Db._
import java.awt.Color
import scala.swing.event.KeyPressed
import scala.swing.event.ButtonClicked
import javax.swing.border.TitledBorder

/**
 * Project name: scabio
 * Date: 9/5/13
 * Time: 1:41 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object SimpleTimerGuiApp extends SimpleSwingApplication with Publisher{


  def top: Frame = new Frame{
    title= "DYNAMIC PROGRAMMING (DNA)"
    preferredSize = new Dimension(1300, 750)

    contents = new BorderPanel{
      val (sTextField1, sTextField2, lenTextField, modeComboBox) = (new MyTextField, new MyTextField,
        new TextField("200"){
          font = Db.monoFont
          tooltip = "Sequences length"
          listenTo(keys)
          reactions += {
            case e: KeyReleased =>
              val tail = e.key.toString
              if(!tail.matches("\\d")) text = text.toUpperCase.replaceAll(tail+"$", "")
              if(text.matches("\\d{4,}")){
                mxCheckBox.selected = false
                mxCheckBox.tooltip = "Warning: checking this checkbox will considerably slow down the computation."
              }else{
                mxCheckBox.selected = true
                mxCheckBox.tooltip = "Check this checkbox the show the matrix."
              }

            case KeyPressed(_, Key.Enter, _, _) => calc
          }
        },
        new ComboBox(dps)
      )

      var firstEntry = true
      val start = "#################### START ####################"
      def calc{
        //print the beginning of the computation
        val start = if(firstEntry){
          firstEntry = false; this.start
        }else "\n"+this.start
        println(start)

        //the computation itself
        val _len = lenTextField.text
        if(_len.matches("\\d+")){
          val len = _len.toInt
          val (_s1, _s2) = (new StringBuilder, new StringBuilder)
          if(modeComboBox.selection.item == dps(0)) (0 until len).map(_ => {
            _s1 += Random.shuffle(aas).head
            _s2 += Random.shuffle(aas).head
          })
          else (0 until len).map(_ => _s1 += Random.shuffle(aas).head)

          val (s1, s2) = (_s1.mkString, _s2.mkString)
          sTextField1.text = s1
          sTextField2.text = s2

          Db.calc(s1, s2)
        }else print("\tInvalid sequence length")
      }



      layout(new BorderPanel{ //NORTH - START
        layout(new BoxPanel(Vertical){
          contents += lenTextField
          contents += Swing.VStrut(5)
          contents += modeComboBox
        }) = West

        layout(new BoxPanel(Vertical){
          contents += sTextField1
          //contents += Swing.VStrut(5)
          contents += sTextField2

          border = Swing.EmptyBorder(0, 10, 0, 10)
        }) = Center

        layout(new BoxPanel(Vertical){
          contents += mxCheckBox
          contents += new Button("CALC."){
            foreground = Color.BLUE
            reactions += {
              case e: ButtonClicked => calc
            }
        }}) = East
      }) = North //NORTH - END


      layout(new BoxPanel(Vertical){
        contents += Swing.VStrut(20)

        contents += new MySplitPane(Vertical, 0.5){
          leftComponent = new MySplitPane(Horizontal, 0.7){
            border = new TitledBorder(seq)
            topComponent = new ScrollPane{contents = seqMxTextArea}
            bottomComponent = new ScrollPane{contents = seqRsTextArea}
          }

          rightComponent = new MySplitPane(Horizontal, 0.7){
            border = new TitledBorder(con)
            topComponent = new ScrollPane{contents = conMxTextArea}
            bottomComponent = new ScrollPane{contents = conRsTextArea}
          }
        }
      }) = Center
    }
  }
}
