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
package net.gumbix.bioinf.string.dotplot

import net.gumbix.dynpro.Idx
import net.gumbix.ui.{MatrixPanel, Data}
import swing.{MainFrame}

/**
 * A tool for dot plots. 
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */
object DotPlot {
  def main(args: Array[String]) {
    new DotPlot(args(0), args(1), args(2))
  }
}

/**
 * @param s1 First string.
 * @param s2 Second string.
 * @param windowTitle The title of the frame.
 */
class DotPlot(val s1: String, val s2: String, windowTitle: String) {

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