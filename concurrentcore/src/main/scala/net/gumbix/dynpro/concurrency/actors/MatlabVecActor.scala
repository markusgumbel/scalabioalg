package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.Messages.DONE
import scala.actors.Actor
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 2:00 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class represents the slave actor used during the conversion of the matrix into an
 * array interpretable by the programming language "MatLab".
 * @param mxActor see AbsSlaveActor.scala
 * @param row matrix(row) =: vector to be converted by this slave actor
 */
protected[actors] final class MatlabVecActor(mxActor: MatlabActor, row: Int)
extends AbsSlaveActor(mxActor){

  override protected def startInternalActors{
    case class LoopPair(loopStart: Int, loopEnd: Int)
    val (loopEnd, loopPairs) = (mxActor.slModVecLen, new ListBuffer[LoopPair]())
    var (start, end) = (0, 0)

    while(end < loopEnd){
      /*The first condition is to avoid having too small sub vectors.*/
      start = end; end += mxActor.range
      end = if ((end + mxActor.range/2) > loopEnd || end > loopEnd) loopEnd else end
      raiseCounter//-> counter += 1

      loopPairs += LoopPair(start, end)
    }

    loopPairs.foreach(pair => {new Actor{ //anonymous sub slave actor
      override def act{
        for(i <- pair.loopStart until pair.loopEnd) mxActor.convert(Idx(row, i)) //convert a part of the matrix

        MatlabVecActor.this ! DONE
      }
    }.start })
  }


  override protected def ePair = new EPair(row, 0)


  override def act{
    startInternalActors

    loopWhile(keepLoopAlive){
      react{
        case DONE => reduceCounter //-> counter -= 1
      }
    }andThen mxActor ! DONE
  }

}
