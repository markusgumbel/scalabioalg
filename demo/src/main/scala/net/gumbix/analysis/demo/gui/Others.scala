package net.gumbix.analysis.demo.gui

import scala.swing.{TextField, SplitPane, TextArea, Orientation, Label}
import java.awt.{Color, Font}
import net.gumbix.bioinf.string.alignment.{AlignmentMode, Alignment}
import net.gumbix.dynpro.concurrency.ConClass._
import net.gumbix.dynpro.concurrency.ConMode._
import net.gumbix.bioinf.string.alignment.AlignmentStep._
import net.gumbix.bioinf.hmm.Viterbi
import net.gumbix.analysis.ViterbiFigures._
import scala.swing.event.{Key, KeyPressed, KeyReleased}

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
  val (br, mx, rs, seq, con, nbr, dur, monoFont, backGround, dps, aas, map) = (
    "\n", "MATRIX", "RESULT", "SEQUENTIAL", "CONCURRENT", "Number of changes: ", "\n\nDurations:\n",
    Font.decode(Font.MONOSPACED + "-15"), new Color(240, 240, 240),
    List("GLOBAL ALIGNMENT", "HIDDEN MARKOV MODEL"),
    List('A', 'C', 'G', 'T'), Map(INSERT -> -1, DELETE -> -1, MATCH -> 0, SUBSTITUTION -> -1)
  )

  val (seqMxTextArea, seqRsTextArea, conMxTextArea, conRsTextArea) = (
    MyTextArea(mx), MyTextArea(rs), MyTextArea(mx), MyTextArea(rs)
  )

  private class ConAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val (config, values) = (setConfig(LEFT_UP, EVENT), map)
  }
  private class SeqAlign(s1: String, s2: String) extends Alignment(s1, s2, AlignmentMode.GLOBAL){
    override val values = map
  }

  private class ConViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP){
    override val config = setConfig(UP, EVENT)
  }
  private class SeqViterbi(s: String) extends Viterbi(s, alphabet.toArray, states.toArray, transP, emmP)


  def calc(s1: String, s2: String){
    print(" --> calculating the solution...")
    def p{print(" --> the solution is done --> creating the table...")}

    val (
      seqSim, seqAlign, seqMatrix, seqDecision, seqDur,
      conSim, conAlign, conMatrix, conDecision, conDur
    ) = if(s2.nonEmpty){
      val (seqDp, conDp) = (new SeqAlign(s1, s2), new ConAlign(s1, s2))
      val (seqSol, conSol) = (seqDp.solution, conDp.solution)
      p

      (
        nbr + -seqDp.similarity + br,
        seqDp.makeAlignmentString(seqSol) + br,
        br+br + seqDp.mkMatrixString(seqSol) + br,
        seqSol.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2),
        dur + seqDp.getDurations.mkString(br),

        nbr + -conDp.similarity + br,
        conDp.makeAlignmentString(conSol) + br,
        br+br + conDp.mkMatrixString(conSol) + br,
        conSol.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2),
        dur + conDp.getDurations.mkString(br)
      )
    }else{
      val (seqDp, conDp) = (new SeqViterbi(s1), new ConViterbi(s1))
      val (seqSol, conSol) = (seqDp.solution, conDp.solution)
      p

      ( "", "",
        br+br + seqDp.mkMatrixString(seqSol) + br,
        seqSol.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2),
        dur + seqDp.getDurations.mkString(br),

        "", "",
        br+br + conDp.mkMatrixString(conSol) + br,
        conSol.map(e => e.decision.toString).reduceLeft((s1, s2) => s1 + s2),
        dur + conDp.getDurations.mkString(br)
        )
    }

    println(" --> the creation is done.")
    seqMxTextArea.text = mx+":" + seqMatrix
    seqRsTextArea.text = seqSim + seqAlign + seqDecision + seqDur
    conMxTextArea.text = mx+":" + conMatrix
    conRsTextArea.text = conSim + conAlign + conDecision + conDur
  }

}
