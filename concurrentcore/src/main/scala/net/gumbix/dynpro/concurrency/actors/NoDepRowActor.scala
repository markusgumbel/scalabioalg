package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency.{msgInternalDone2, Messages, msgComputationDone}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 2:00 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class NoDepRowActor[ComputeMatrixReturnType]
          (noDepActor: NoDepAbsActor[ComputeMatrixReturnType], row: Int,
           mx: Array[Option[Double]])
          extends AbsSlaveActor(noDepActor){

  override def startInternalActors {
    case class Pair(loopStart: Int, loopEnd: Int)
    val (pairMap, range, loopEnd) = (Map[Int, Pair](), noDepActor.intervalSize, mx.length)
    var (start, end) = (0, 0)

    while(end < loopEnd){
      (start, end) = (end, end + range)
      end = if (end < loopEnd) end else loopEnd
      add //-> counter += 1
      pairMap + (getCounter -> Pair(start, end))
    }

    for(key <- pairMap.keys) yield{
      val a = actor{
        react{
          case Messages.start =>
            val list = new ListBuffer[ComputeMatrixReturnType]()
            for(i <- pairMap(key).loopStart until pairMap(key).loopEnd)
              list += noDepActor.handleSubMatrix(mx(i))

            this ! msgInternalDone2(key, list)
        }
      }

      a ! Messages.start
    }

  }


  override def ePair = new EPair(row, 0)


  override def act{
    startInternalActors
    val (mxListBuffer, listMap) =
      (new ListBuffer[ComputeMatrixReturnType](), Map[Int, ListBuffer[ComputeMatrixReturnType]]())

    def _andThen{
      for(key <- listMap.keys.toList.sorted) mxListBuffer ++= listMap(key)
      noDepActor ! msgComputationDone(row, mxListBuffer.toArray)
    }

    loopWhile(keepLoopAlive){
      react{
        case msgInternalDone2(key, list) =>
          listMap + (key -> list)
          subtract //-> counter -= 1
      }
    }andThen( _andThen )
  }

}
