package net.gumbix.dynpro.concurrency

import net.gumbix.dynpro.{PathEntry, Idx}
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/22/13
 * Time: 12:13 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[concurrency] object Messages {
//symbol messages
  val computationNotDone = 'NotTotallyComputedYet
  val computationDone = 'MatrixCellActorComputationDone
  val start = 'start
  val die = 'Die
}


//Messages
////All
protected[concurrency] case class msgException(key: Int, pointer: Int)

////MatrixActor & MatrixCellActor
protected[concurrency] case class msgGetValues(missingValIndexes: ListBuffer[Idx])
protected[concurrency] case class msgAckGetValues(values: ListBuffer[Double])
protected[concurrency] case class msgUpdateMatrix(idx: Idx, newValue: Double)
protected[concurrency] case class msgCompute(mx: Array[Array[Option[Double]]], pointer: Int)

////Others
protected[concurrency] case class msgComputationDone[ComputeMatrixReturnType](row: Int, mx: Array[ComputeMatrixReturnType])
protected[concurrency] case class msgInternalDone[Decision](pathList: ListBuffer[PathEntry[Decision]])
protected[concurrency] case class msgInternalDone2[ComputeMatrixReturnType](key: Int, list: ListBuffer[ComputeMatrixReturnType])
protected[concurrency] case class msgSolutionReady[Decision](key: Int, pathListsList: ListBuffer[ListBuffer[PathEntry[Decision]]])

