package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.{PathEntry, Idx}
import net.gumbix.dynpro.concurrency.Messages.DONE
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
/**
 * This class represents the slave actor during the path finding stage
 * @param solActor see AbsSlaveActor.scala
 * @param key map(key) =: pin point index used to find the paths
 * @tparam Decision see DynProConfig.scala
 */
protected[actors] final class SolutionSubActor[Decision]
  (solActor: SolutionActor[Decision], key: Int)
  extends AbsSlaveActor(solActor){

  override protected def startInternalActors =
    for(idx <- solActor.getIdxList(key)){
      raiseCounter //-> counter += 1
      class SubSlAc(slAc: Actor) extends Actor{
        //sub slave actor
        override def act{
          slAc ! solActor.getPath(idx)
        }
      }

      new SubSlAc(this).start
  }


  override def ePair = new EPair(key, 0)


  override def act{
    startInternalActors
    val pathList = ListBuffer[ListBuffer[PathEntry[Decision]]]()
    /**
    def afterLoopWhile{
      solActor ! MsgRelSolListDone(key, pathListsList)
      exit
    }
    */
    def afterLoopWhile{
      solActor.updatePathListMap(key, pathList)
      solActor ! DONE
    }

    loopWhile(keepLoopAlive){
      react{
        case path: ListBuffer[PathEntry[Decision]] =>
          pathList += path
          reduceCounter //-> counter -= 1
      }
    }andThen afterLoopWhile
  }

}
