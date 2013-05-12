package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency._
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.msgComputationDone
import net.gumbix.dynpro.concurrency.msgException

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 2:34 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] abstract class NoDepAbsActor[ComputeMatrixReturnType]
                                      (mx: Array[Array[Option[Double]]], noDepRowActorAm: Int)
                                extends Actor with AbsMasterActor{

  val matrix: Array[Array[ComputeMatrixReturnType]] = Array.ofDim(mx.length, mx(0).length)


  override final def actReact{
    react{
      case msgException(row, 0) => restartSlave(row)

      case msgComputationDone(row, mxRow) =>
        congestionControl
        matrix(row) = (mxRow asInstanceOf)[Array[ComputeMatrixReturnType]]
    }
  }

  override protected def amPair = AmPair(mx.length, noDepRowActorAm)


  override protected def startNewSlave(row: Int){
    val rowActor = new NoDepRowActor(getNewClassObject, row, mx(row))
    link(rowActor)
    //start row computation with exactly one row of the  "mx".
    rowActor.start
  }


  override def getComputedResult: Array[Array[ComputeMatrixReturnType]] = matrix


  protected[actors] abstract def handleSubMatrix(subMatrixCell: Option[Double]): ComputeMatrixReturnType
  protected abstract def getNewClassObject: NoDepAbsActor[ComputeMatrixReturnType]
  //protected[actors] abstract def getNewClassObject(mx: Array[Array[Option[Double]]], noDependencyRowActorAmount: Int)
    //                              : NoDepAbsActor[ComputeMatrixReturnType]
  /*From concurrency.IMaster.scala
    override def stage
   */


}

