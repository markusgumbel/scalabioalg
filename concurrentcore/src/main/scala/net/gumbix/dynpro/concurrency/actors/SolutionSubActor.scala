package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.{PathEntry, Idx}
import net.gumbix.dynpro.concurrency.{Debugger, Messages, msgSolInterDone, msgSolDone}
import scala.collection.mutable.ListBuffer
import scala.actors.Actor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/9/13
 * Time: 5:15 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class SolutionSubActor[Decision]
  (solActor: SolutionActor[Decision], mapKey: Int, idx: Idx)
  extends AbsSlaveActor(solActor){

  override protected def startInternalActors = for(idx <- solActor.getIdxList(idx)) yield{
    raiseCounter //-> counter += 1
    class A(master: Actor) extends Actor{
      //Anonymous class
      override def act{
        react{
          case Messages.start => master ! msgSolInterDone(solActor.getPathList(idx))
        }
      }
    }

    val a = new A(this)
    a.start
    a ! Messages.start
  }


  override def ePair = new EPair(mapKey, 0)


  override def act{
    startInternalActors
    val pathListsList = ListBuffer[ListBuffer[PathEntry[Decision]]]()

    loopWhile(keepLoopAlive){
      react{
        case msgSolInterDone(pathList) =>
          pathListsList += pathList.asInstanceOf[ListBuffer[PathEntry[Decision]]]
          reduceCounter //-> counter -= 1
      }
    }andThen(solActor ! msgSolDone(mapKey, pathListsList))
  }

}
