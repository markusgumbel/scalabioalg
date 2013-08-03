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
 * @param convert see DynProConfig.scala
 * @param range
 */
protected[concurrency] final class MatlabActor(
  slModAm: Int, val slModVecLen: Int, val convert: Idx => Unit, val range: Int)
  extends AbsMasterActor{

  override protected def eTerms = ETerms("Matrix conversion", "Row", "")

  override protected val getPoolSize = PoolSize(slModAm, slModAm * (slModVecLen/range + 1))


  override protected def startNewSlMod(row: Int) {
    new MatlabVecActor(this, row).start
  }

  override protected def actReact{
    react{
      case MsgException(e, row, 0) => handleException(e, row, 0)

      case DONE => congestionControl
    }
  }

  override protected def ackStart: Symbol = DONE

}
