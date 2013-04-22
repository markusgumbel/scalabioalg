package net.gumbix.paradynpro.actors

import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 12:13 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
object Messages {
  //symbol messages
  protected[actors] val symbol = Array(
    'NotTotallyComputedYet,
    'CellActorComputationDone)
}


//Messages
case class msgGetValues(missingValIndexes: Array[Idx])
case class msgAckGetValues(values: Array[Double])
case class msgUpdateMatrix(idx: Idx, newValue: Double)
