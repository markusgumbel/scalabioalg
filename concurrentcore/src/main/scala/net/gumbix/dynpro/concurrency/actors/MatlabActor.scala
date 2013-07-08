package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.MsgException
import net.gumbix.dynpro.concurrency.Messages.DONE
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/1/13
 * Time: 8:28 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class represents the master actor used during the conversion of the matrix into an
 * array interpretable by the programming language "MatLab".
 * @param _getDim see AbsSlaveActor.scala
 * @param convert see DynProConfig.scala
 * @param range
 */
protected[concurrency] final class MatlabActor(
  _getDim:() => (Int, Int), val convert: Idx => Unit, val range: Int)
  extends AbsMasterActor(_getDim){

  override protected def getPoolSize =
    PoolSize(getDim._1, getDim._1 * (getDim._2/range + 1))

  override def eTerms = ETerms("Matrix conversion", "Row", "")
  //private val matrix: Array[Array[Double]] = Array.ofDim(getMatrix.length, getMatrix(0).length)

  override protected def restartSlMod(row: Int){slModules(row).restart}

  override protected def startNewSlMod(row: Int) {
    val actor = new MatlabVecActor(this, row)
    actor.start
    slModules += actor
  }

  override protected def actReact{
    react{
      case MsgException(e, row, 0) => handleException(e, row, 0)

      case DONE => congestionControl
    }
  }

  override protected def ackStart: Symbol = DONE

}
