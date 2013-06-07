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
  val startsSlAc = 'startSubSlaveActor
  val start = 'startMasterActor
}


////Used in more than one communication
protected[concurrency] case class MsgException(e: Exception, key: Int, pointer: Int)
protected[concurrency] case class MsgMxDone(matrix: Array[Array[Option[Double]]])
protected[concurrency] case class MsgCostPairs(costPairs: ListBuffer[CostPair])
protected[concurrency] case class MsgRegister(channels: ListBuffer[Int])


////~NoDepActor & NoDepRowActor
protected[concurrency] case class MsgEmpVecDone(row: Int, vector: ListBuffer[Option[Double]])
protected[concurrency] case class MsgMatVecDone(row: Int, vector: ListBuffer[Double])
protected[concurrency] case class MsgMatDone(matrix: Array[Array[Double]])
protected[concurrency] case class MsgNoDepInterDone[MxDT](key: Int, list: ListBuffer[MxDT])


////MatrixActor & MatrixVectorActor
/**
 * This case class is invoked in the MatrixVectorActor class to request
 * the values missing in the local matrix.
 * @param mValIdxList =: missingValIndexesList
 *                   The index list of cells without values (value =: None).
 */
protected[concurrency] case class MsgGetValues(mValIdxList: ListBuffer[Idx])

/**
 * This case class is invoked in the MatrixActor class to answer to one of its
 * MatrixVectorActor class slaves "msgGetValues" request.
 * @param values The list of the values requested.
 */
protected[concurrency] case class MsgAckGetValues(values: ListBuffer[Option[Double]])

/**
 * This case class is used by the MasterCellActor class to send newly computed
 * values to its MasterActor class master.
 * @param idx The index where the values should be stored.
 * @param newValue The newly computed value.
 */
protected[concurrency] case class MsgUpdateMatrix(idx: Idx, newValue: Double)


////MxLeftUpActor & MxLeftUpVecActor
protected[concurrency] case class MsgRow(i:Int, row: Array[Option[Double]])


////MxUpActor & MxUpVecActor
protected[concurrency] case class MsgCol(j:Int, costPairs: ListBuffer[CostPair])


////SolutionActor & SolutionSubActor
protected[concurrency] case class MsgRelSolDone[Decision](pathList: ListBuffer[PathEntry[Decision]])
protected[concurrency] case class MsgRelSolListDone[Decision](key: Int, pathListsList: ListBuffer[ListBuffer[PathEntry[Decision]]])
protected[concurrency] case class MsgSolDone[Decision](pathList: ListBuffer[PathEntry[Decision]])

