# Overview

This project provides algorithms written in Scala which are used in the bioinformatics domain.

Highlights so far

* generic (2D) dynamic programing algorithm (see `net.gumbix.dynpro.DynPro`)
* Global, semi-local and local pairwise alignment (see `net.gumbix.bioinf.string.alignment.Alignment`)
* Multiple alignment
 * star algorithm, see `net.gumbix.bioinf.string.alignment.StarAlignment`
 * Clustal variant, see `net.gumbix.bioinf.string.alignment.Clustal`
* Pattern recognition with the Viterbi-algorithm (hidden markov model, see `net.gumbix.bioinf.hmm.Viterbi`)
* RNA 2D struct viewer based on Nussinov algorithm (see `net.gumbix.bioinf.struct.RNAStruct2DViewer`)
* Greedy superstring algorithm for sequence assembly (see `net.gumbix.bioinf.string.seq.GreedySuperstring`)
* and many more...

# Getting Started

*More documentation will come soon.* Please refer to [project home page](http://mi.informatik.hs-mannheim.de/gumbel/en/forschung/scabio/) or the to the [wiki pages](https://github.com/markusgumbel/scalabioalg/wiki) for more information. A good start is to read the preliminary [manual](http://mi.informatik.hs-mannheim.de/gumbel/files/scabio-manual.pdf) or the generated scaladoc API documentation. See installation below where to find it.

There are two important branches:

* master: The main branch that contains the sequential dynamic programming algorithm.
* conc_dp: An experimental branch that uses a concurrent dynamic programming approach.

# Requirements

* [GIT](http://git-scm.com/)
* [Maven 3](http://maven.apache.org/)

# Installation

The project uses Maven 3 as the build tool. Check out the project and run the Maven goal `mvn site`. This will generate all the relevant artifacts. The scaladoc can be found in ./target/site/index.html. More details are described int the [manual](http://mi.informatik.hs-mannheim.de/gumbel/files/scabio-manual.pdf).

# Mailing Lists

None yet. :-)

# History

The software was originally developed for a lecture on [bioinformatics](http://mi.informatik.hs-mannheim.de/gumbel/aktuelle-vorlesungen/bioinformatik-bim/). Thus, this software was mainly supposed to be for academic use. However, I believe that part of it might be of common interest. Let's see how this project relates to [Biojava](http://biojava.org/wiki/Main_Page) and other projects like [bioscala](https://github.com/bioscala/bioscala).    