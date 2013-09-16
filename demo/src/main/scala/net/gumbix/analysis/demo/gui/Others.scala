package net.gumbix.analysis.demo.gui

import scala.swing.{TextField, SplitPane, TextArea, Orientation, CheckBox}

import java.awt.{Color, Font}
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.KeyReleased
import scala.actors.Actor

import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.bioinf.hmm.Viterbi
import net.gumbix.analysis.ViterbiFigures._
import net.gumbix.dynpro.{PathEntry, DynPro}
import net.gumbix.dynpro.concurrency.Messages._


/**
 * An algorithm for data transfer.
 * Project name: scabio
 * Date: 9/5/13
 * Time: 4:03 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
case class MyTextArea(_text: String) extends TextArea(_text + " ..."){
  editable = false
  font = Db.monoFont
  background = Db.backGround
}

class MySplitPane(o: Orientation.Value, loc: Double) extends SplitPane(o){
  continuousLayout = true
  oneTouchExpandable = true
  resizeWeight = loc
}

class MyTextField extends TextField{
  foreground = Color.blue
  background = Db.backGround
  font = Db.monoFont

  private val (_text, nonKeyFlags) = (new StringBuilder, List(Key.withName("Pos 1"), Key.End, Key.Left, Key.Right))


  listenTo(keys)
  reactions += {
    //The combination of both events make this TextField almost uneditable,
    //however preserving the ability of using the left and right arrows.
    case e: KeyPressed => //this event is fired before the text attribute is internaly updated
      _text.clear
      _text ++= text

    case e: KeyReleased => //this event is fired right after the text attribute is internaly updated
      if(!nonKeyFlags.contains(e.key)) text = _text.mkString
  }
}

object Db{
  val (br, mx, rs, seq, con, s, nbr, dur, monoFont, backGround, dps, aas, map) = (
    "\n", "MATRIX", "RESULT", "SEQUENTIAL", "CONCURRENT", " sec",
    "Number of changes: ", "\n\nDurations:\n",
    Font.decode(Font.MONOSPACED + "-15"), new Color(240, 240, 240),
    List("GLOBAL ALIGNMENT", "HIDDEN MARKOV MODEL"),
    List('A', 'C', 'G', 'T'), Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)
  )
  val (_mx, _s) = (mx+":" +br+br, s+br)

  val (seqMxTextArea, seqRsTextArea, conMxTextArea, conRsTextArea, mxCheckBox) = (
    MyTextArea(mx), MyTextArea(rs), MyTextArea(mx), MyTextArea(rs),
    new CheckBox("Show the matrix"){selected = true}
  )

  /********** ACTOR CLASSES - START **********/
  private class DpActor(dp: DynPro[Any], rsTextArea: MyTextArea, mxTextArea: MyTextArea)
  extends Actor{
    private val className = "\t" + dp.getClass.getSimpleName

    protected final lazy val sol = {
      println(className + ": calculating the solution...")
      val sol = dp.solution
      println(className + ": the solution has been calculated.")
      sol
    }
    protected lazy val prefix = ""

    def act{
      //first delete the old result if there was ever one.
      rsTextArea.text = ""
      mxTextArea.text = ""

      //after that compute the new one.
      rsTextArea.text = prefix +
        sol.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2) +
        dur + dp.getDurations.mkString(_s)+s

      if(mxCheckBox.selected){
        println(className + " : creating the matrix table...")
        mxTextArea.text = _mx + dp._mkMatrixString(sol) + br
        println(className + " : the creation is done.")
      }
    }
  }

  private class GlobAlignActor(dp: Alignment, rsTextArea: MyTextArea, mxTextArea: MyTextArea)
  extends DpActor(dp.asInstanceOf[DynPro[Any]], rsTextArea, mxTextArea){
    override lazy val prefix = nbr + -dp.similarity + br +
      dp.makeAlignmentString(sol.asInstanceOf[List[PathEntry[AlignmentStep]]]) + br
  }

  private class ViterbiActor(dp: Viterbi, rsTextArea: MyTextArea, mxTextArea: MyTextArea)
  extends DpActor(dp.asInstanceOf[DynPro[Any]], rsTextArea, mxTextArea)
  /********** ACTOR CLASSES - END **********/


  private class ConAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val config = setConfig(LEFT_UP, EVENT)
  }
  private class SeqAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL)

  private class ConViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT) //don't worry this isn't an error
  }
  private class SeqViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP)


  def calc(s1: String, s2: String){
    //Concurrently get the results of the sequential- and the concurrent dyn pro alg.
    if(s2.nonEmpty){
      new GlobAlignActor(new SeqAlign(s1, s2), seqRsTextArea, seqMxTextArea).start
      new GlobAlignActor(new ConAlign(s1, s2), conRsTextArea, conMxTextArea).start
    }else{
      new ViterbiActor(new SeqViterbi(s1), seqRsTextArea, seqMxTextArea).start
      new ViterbiActor(new ConViterbi(s1), conRsTextArea, conMxTextArea).start
    }
  }
}
