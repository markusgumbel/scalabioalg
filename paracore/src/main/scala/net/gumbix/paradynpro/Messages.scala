package net.gumbix.paradynpro

import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 12:13 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[paradynpro] object Messages {
  //symbol messages
  val symbol = Array(
    'NotTotallyComputedYet,
    'CellActorComputationDone,
    'MatrixTotallyComputed)
}


//Messages
protected[paradynpro] case class msgGetValues(missingValIndexes: Array[Idx])
protected[paradynpro] case class msgAckGetValues(values: Array[Double])
protected[paradynpro] case class msgUpdateMatrix(idx: Idx, newValue: Double)
