package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.{PathEntry, Idx}
import net.gumbix.dynpro.concurrency.MsgRelSolListDone
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
    class SubSlAc(slAc: Actor) extends Actor{
      //sub slave actor
      override def act{
        slAc ! solActor.getPathList(idx)
      }
    }

    new SubSlAc(this).start
  }


  override def ePair = new EPair(mapKey, 0)


  override def act{
    startInternalActors
    val pathListsList = ListBuffer[ListBuffer[PathEntry[Decision]]]()
    /**
    def afterLoopWhile{
      solActor ! MsgRelSolListDone(mapKey, pathListsList)
      exit
    }
    */

    loopWhile(keepLoopAlive){
      react{
        case pathList: ListBuffer[PathEntry[Decision]] =>
          pathListsList += pathList
          reduceCounter //-> counter -= 1
      }
    }andThen(solActor ! MsgRelSolListDone(mapKey, pathListsList)) //(afterLoopWhile)
  }

}
