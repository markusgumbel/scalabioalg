package net.gumbix.dynpro.concurrency.actors

import scala.collection.mutable.ListBuffer
import net.gumbix.dynpro.concurrency._
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
    val loopEnd = mx.length
    var (pairMap, start, end) = (Map[Int, LoopPair](), 0, 0)

    while(end < loopEnd){
      start = end; end += mxActor.range
      /*
      The first condition is to avoid having to smaller sub vectors.
       */
      end = if ((end + mxActor.range/2) > loopEnd || end > loopEnd) loopEnd else end
      raiseCounter //-> counter += 1
      pairMap += (getCounter -> LoopPair(start, end))
    }


    for(key <- pairMap.keys){
      class A(master: Actor) extends Actor{
        //Anonymous class
        override def act{
          react{
            case Messages.start =>
              val list = new ListBuffer[MxDT]()
              for(i <- pairMap(key).loopStart until pairMap(key).loopEnd)
                list += mxActor.handleCell(mx(i))

              master ! msgNoDepInterDone[MxDT](key, list)
          }
        }
      }

      val a = new A(this)
      a.start
      a ! Messages.start
    }

  }


  override protected def ePair = new EPair(row, 0)


  override def act{
    startInternalActors

    val mxListBuffer = new ListBuffer[MxDT]()
    var listMap = Map[Int, ListBuffer[MxDT]]()

    def _andThen{
      for(key <- listMap.keys.toList.sorted) mxListBuffer ++= listMap(key)
      mxActor.sendMsg(row, mxListBuffer)
      //mxActor ! msgCompDone(row, mxListBuffer.toArray)

    }

    loopWhile(keepLoopAlive){
      react{
        case msgNoDepInterDone(key, list) =>
          listMap += (key -> list.asInstanceOf[ListBuffer[MxDT]])
          reduceCounter //-> counter -= 1
      }
    }andThen( _andThen )
  }

}
