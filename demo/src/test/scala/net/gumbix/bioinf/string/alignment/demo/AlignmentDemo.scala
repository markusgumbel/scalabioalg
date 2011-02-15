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
package net.gumbix.bioinf.string.alignment.demo

import junit.framework.TestCase
import net.gumbix.bioinf.string.alignment.AlignmentMode._
import collection.immutable.HashMap
import net.gumbix.bioinf.string.alignment.{Alignment, AlignmentMode}

class AlignmentDemo extends TestCase {
  val seqs = HashMap[String, Tuple3[String, String, String]](
    "Boeck-p88" -> ("AAAAACTCTCTCT", "GCGCGCGCAAAAA", "Böckenhauer, p. 88"),
    "Huett-p151" -> ("CGATCCTGT", "CATCGCCTT", "Hütt, p. 151"),
    "Huett-p148" -> ("KIAQYKRECPSIFAWEIRDRL",
            "KIAQYKRECPNIPSVSSINSIFAWEIRDRL",
            "Hütt p. 148. Einschub"),
    "Huett-p151b" -> ("MQNSHSGVNQLGGVFVNGRPLPDSTRQKIVELAHSGARPCDISRILQTHADAKVQVLDNENVSNGCVSKILGRYYETGSIRPRAIGGSKPRVATPEVVSKIAQYKRECPSIFAWEIRDRLLSEGVCTNDNIPSVSSINRVLRNLASEKQQMGADGMYDKLRMLNGQTGSWGTRPGWYPGTSVPGQPTQDGCQQQEGGGENTNSISSNGEDSDEAQMRLQLKRKLQRNRTSFTQEQIEALEKEFERTHYPDVFARERLAAKIDLPEARIQVWFSNRRAKWRREEKLRNQRRQASNTPSHIPISSSFSTSVYQPIPQPTTPVSSFTSGSMLGRTDTALTNTYSALPPMPSFTMANNLPMQPPVPSQTSSYSCMLPTSPSVNGRSYDTYTPPHMQTHMNSQPMGTSGTTSTGLISPGVSVPVQVPGSEPDMSQYWPRLQ",
            "MFTLQPTPAAIGSVPWSAGSLIERLPTLDDMNHKDNVLAMRNLPCLGVAGGSGLGGIGGKAAATAAMEAAADATTAPQPPHSTSSYFTTTYYHLTDDECHSGVNQLGGVFVGGRPLPDSTRQKIVELAHSGARPCDISRILQVSNGCVSKILGRYYETGSIRPRAIGGSKPRVATAEVVSKISQYKRECPSIFAWEIRDRLLQESVCTNDNIPSVSSINRVLRNLAAQKEQQNTGTSSSNPNPNPNPNPSHAAASGNGSSNNGNSSSNGNSVSGANGVGPSSTNDLIQTATPLNSSESGGASNSGEGSEQESIYEKIRMLNTQQASTLDSAISTTGAPPVTHGQLMTSVPSNFSPHSHSSHPIHTHGHGHQQQQSWPTRHYPTGSWYAAPLNGSELVSSPGVISVTGYGNGGLTAASGLVPGHPLTPPSDLINIGGPSARLGNCTNSADDVMLKKELDGHQSDETGSGEGDNSNGGASNIGTSEDDQARLILKRKLQRNRTSFTNDQIDSLEKEFERTHYPDVFARERLAGKIGLPEARIQVWFSNRRAKWRREEKLRNQRRTPNSTGTNGTSSSTSATTSLTDSPNSLGGCSSLLAGSAGSGLNGLASPNPIATPTGGSETTDAHHTASGGAHIRSGTHDIGNACSPGLGIADQRHQHHHHVPPHTLVPSISPRLNFNSGFSSSMSAMYSNMHHNAISMSEGYGSVSSIPNFGHPTVGSLAPPSPVMQQRDLTPPSMYPCHMPLRPPPIPPHQLVGVGSATNDSAGVSSSPLQSNPHAASGTVASNNATGYESLSAYSLPPPPPASTAANHIPSGHHHSMEARQATCSPSLMNSGGSVHGVGHGSGFGSESISPAMPSYAHMSYNYAAAASGVASSGGANPAAAINSHGSGKQQFFASCFYSPWA",
            "Vergleich PAX-6 (Genbank: NP_038655) mit eyeless (Genbank: XP_002059744)"),
    "Hütt p. 148b" -> ("KIQYKREPNIPSVSLINSLFAWEIRDRI", "KAQYRRECMIFVWEINRL", "nach Hütt p. 148.")
    )

  /**
   * Perform all kind of alignments.
   */
  def testAllAlignments() {
    for ((s1, s2, comment) <- seqs.values;
         mode <- AlignmentMode.values) {
      doAligmentDP(mode, s1, s2, comment)
    }
  }

  def doAligmentDP(mode: AlignmentMode, s1: String, s2: String, comment: String) {
    println()
    println("---------------------------------")
    println("Alignment: method = " + mode + "; " + comment)
    println()
    println("s1 = " + s1 + ", s2 = " + s2)
    val dp = new Alignment(s1, s2, mode)
    val solution = dp.solution
    println("sim = " + dp.similarity)
    println()
    println(dp.makeAlignmentString(solution))
    println()
    if (s1.size < 100 && s2.size < 100) {
      println(dp.mkMatrixString(solution))
      println()
      solution.foreach(e => print(e.decision))
      // solution.foreach(e => println(e))
      println()
    }
  }
}