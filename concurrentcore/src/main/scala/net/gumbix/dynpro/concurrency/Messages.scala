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
  val start = 'startMasterActor
}


////Used in all master actors
protected[concurrency] case class MsgException(e: Exception, key: Int, pointer: Int)


////~NoDepActor & NoDepRowActor
protected[concurrency] case class MsgEmpVecDone(row: Int, vector: ListBuffer[Option[Double]])
protected[concurrency] case class MsgMatVecDone(row: Int, vector: ListBuffer[Double])
protected[concurrency] case class MsgNoDepInterDone[MxDT](key: Int, list: ListBuffer[MxDT])


////MxLeftUpActor & MxLeftUpVecActor - MxUpActor & MxUpVecActor
protected[concurrency] case class MsgRow(i:Int, row: Array[Option[Double]])
protected[concurrency] case class MsgCol(j:Int, costPairs: ListBuffer[CostPair])


////SolutionActor & SolutionSubActor
protected[concurrency] case class MsgRelSolListDone[Decision](key: Int, pathListsList: ListBuffer[ListBuffer[PathEntry[Decision]]])

