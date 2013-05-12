package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.{PathEntry, Idx}
import scala.actors.Actor
import scala.actors.Actor._
import net.gumbix.dynpro.concurrency.{msgInternalDone, Messages, msgSolutionReady}
import scala.collection.mutable.ListBuffer

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/9/13
 * Time: 5:15 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[actors] final class SolutionSubActor[Decision]
  (solActor: SolutionActor[Decision], mapKey: Int, idxList: ListBuffer[Idx])
  extends AbsSlaveActor(solActor){

  override def startInternalActors = for(idx <- idxList) yield{
    add //-> counter += 1
    val a = actor{
      react{
        case Messages.start => this ! msgInternalDone(solActor.calculateSolution(idx))
      }
    }

    a ! Messages.start
  }


  override def ePair = new EPair(mapKey, 0)


  override def act{
    startInternalActors
    val pathListsList = ListBuffer[ListBuffer[PathEntry[Decision]]]()

    //def _andThen = solActor ! msgSolutionReady(mapKey, pathListsList)

    loopWhile(keepLoopAlive){
      react{
        case msgInternalDone(pathList) =>
          pathListsList += (pathList asInstanceOf)
          subtract //-> counter -= 1
      }
    }andThen( solActor ! msgSolutionReady(mapKey, pathListsList) )
  }

}
