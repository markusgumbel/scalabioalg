package net.gumbix.bioinf.string.dotplot

import net.gumbix.dynpro.Idx
import net.gumbix.ui.{MatrixPanel, Data}
import swing.{MainFrame}

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object DotPlot {
  def main(args: Array[String]) {
    new DotPlot2(args(0), args(1), args(2))
  }
}

// TODO Intellij Bug
class DotPlot2(val s1: String, val s2: String, windowTitle: String) {

  def data(s1: String, s2: String) = {
    var dataList = List[Data]()
    for (i <- 0 until s1.size; j <- 0 until s2.size) {
      if (s1(i) == s2(j)) dataList = Data(Idx(i, j), "") :: dataList
    }
    dataList.toArray
  }

  val top = new MainFrame {
    title = windowTitle
    contents = new MatrixPanel(s1.toArray, s2.toArray, data(s1, s2))
  }

  top.pack()
  top.visible = true
}