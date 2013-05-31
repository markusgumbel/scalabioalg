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
  val compDone = 'MatrixCellActorComputationDone
  val start = 'startInternalActor
  val die = 'Die
}

////All
protected[concurrency] case class msgException(key: Int, pointer: Int)

////~NoDepActor & NoDepRowActor
protected[concurrency] case class msgEmpVecDone(row: Int, vector: ListBuffer[Option[Double]])
protected[concurrency] case class msgMatVecDone(row: Int, vector: ListBuffer[Double])
protected[concurrency] case class msgNoDepInterDone[MxDT](key: Int, list: ListBuffer[MxDT])

////MatrixActor & MatrixVectorActor
/**
 * This case class is invoked in the MatrixVectorActor class to request
 * the values missing in the local matrix.
 * @param mValIdxList =: missingValIndexesList
 *                   The index list of cells without values (value =: None).
 */
protected[concurrency] case class msgGetValues(mValIdxList: ListBuffer[Idx])

/**
 * This case class is invoked in the MatrixActor class to answer to one of its
 * MatrixVectorActor class slaves "msgGetValues" request.
 * @param values The list of the values requested.
 */
protected[concurrency] case class msgAckGetValues(values: ListBuffer[Option[Double]])

/**
 * This case class is used by the MasterCellActor class to send newly computed
 * values to its MasterActor class master.
 * @param idx The index where the values should be stored.
 * @param newValue The newly computed value.
 */
protected[concurrency] case class msgUpdateMatrix(idx: Idx, newValue: Double)


////SolutionActor & SolutionSubActor
protected[concurrency] case class msgSolInterDone[Decision](pathList: ListBuffer[PathEntry[Decision]])
protected[concurrency] case class msgSolDone[Decision](key: Int, pathListsList: ListBuffer[ListBuffer[PathEntry[Decision]]])

