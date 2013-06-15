package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.{ListBuffer, Map}
import net.gumbix.dynpro.concurrency.MsgNoDepInterDone
import scala.actors.Actor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/11/13
 * Time: 2:00 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class NoDepRowActor[MxDT]
          (mxActor: NoDepAbsActor[MxDT], row: Int,
           mx: Array[Option[Double]])
          extends AbsSlaveActor(mxActor){

  override protected def startInternalActors{
    case class LoopPair(loopStart: Int, loopEnd: Int)
    val (loopEnd, pairMap) = (mx.length, Map[Int, LoopPair]())
    var (start, end) = (0, 0)

    while(end < loopEnd){
      start = end; end += mxActor.range
      /*
      The first condition is to avoid having to smaller sub vectors.
       */
      end = if ((end + mxActor.range/2) > loopEnd || end > loopEnd) loopEnd else end
      raiseCounter //-> counter += 1
      pairMap += (getCounter -> LoopPair(start, end))
    }


    for(pair <- pairMap){
      class SubSlAc(slAc: Actor) extends Actor{
        //sub slave actor
        override def act{
          val list = new ListBuffer[MxDT]()
          for(i <- pair._2.loopStart until pair._2.loopEnd)
            list += mxActor.handleCell(mx(i))

          slAc ! MsgNoDepInterDone[MxDT](pair._1, list)
        }
      }
      new SubSlAc(this).start
    }

  }


  override protected def ePair = new EPair(row, 0)


  override def act{
    startInternalActors

    val mxListBuffer = new ListBuffer[MxDT]()
    var listMap = Map[Int, ListBuffer[MxDT]]()

    def afterLoopWhile{
      for(key <- listMap.keys.toList.sorted) mxListBuffer ++= listMap(key)
      mxActor.sendMsg(row, mxListBuffer)
      //exit -> no need @ the moment
    }

    loopWhile(keepLoopAlive){
      react{
        case MsgNoDepInterDone(key, list) =>
          listMap += (key -> list.asInstanceOf[ListBuffer[MxDT]])
          reduceCounter //-> counter -= 1
      }
    }andThen(afterLoopWhile)
  }

}
