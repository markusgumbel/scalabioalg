package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency._
import net.gumbix.dynpro.{Idx, PathEntry}
import scala.collection.mutable.ListBuffer
import math.{max, min}
import net.gumbix.dynpro.Idx
import net.gumbix.dynpro.concurrency.msgException2
import net.gumbix.dynpro.concurrency.msgSolutionReady

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 5:56 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

/**
 *
 * @param idx
 * @param solSubActorAm
 * @param calculateSolution
 * @tparam Decision
 */
protected[concurrency] class SolutionActor[Decision]
  (idx: Idx, solSubActorAm: Int,
   calculateSolution:(Idx, (Idx)=> Boolean) => ListBuffer[PathEntry[Decision]])
  extends Actor with AbsMasterActor{

  val pathListsListMap = Map[Int, ListBuffer[ListBuffer[PathEntry[Decision]]]]()

  lazy val (pathList, idxListsMap: Map[Int, ListBuffer[Idx]]) =
    (calculateSolution(idx),
      //SPLIT the matrix
      { //Map of all the indices lists
        val _map = Map[Int, ListBuffer[Idx]]()
        var (baseIdx, range) = (Idx(-1, -1), intervalSize)

        //collect all the start point within the matrix
        while(range < max(idx.i, idx.j)){
          val idxList = new ListBuffer[Idx]()

          if(range < min(idx.i, idx.j)){
            baseIdx = idx - range
            idxList += baseIdx
            for(z <- 1 until range + 1){
              idxList += (baseIdx +(0, z))
              idxList += (baseIdx +(z, 0))
            }
          }else if(range < idx.i){
            baseIdx = new Idx(idx.i - range, 0)
            for(z <- 0 until idx.j + 1) idxList += (baseIdx +(0, z))
          }else if(range < idx.j){
            baseIdx = new Idx(0, idx.j - range)
            for(z <- 0 until idx.i + 1) idxList += (baseIdx +(z, 0))
          }

          _map + ((range/intervalSize) -> idxList)
          range += intervalSize
        }

        _map
      } )


  /**
   *
   * @param currentIdx
   * @return
   */
  private def break(currentIdx: Idx): Boolean = {
    val limitIdx = idx - intervalSize
    if(currentIdx.i == limitIdx.i || currentIdx.j == limitIdx.j) true
    else false
  }


  /**
   * @see
   */
  protected[actor] def calculateSolution(idx: Idx) = calculateSolution(idx, break)



  override def actReact{
    react{
      case msgException(key, 0) => restartSlave(key)

      case msgSolutionReady(key: Int, pathListBufferLists) =>
        congestionControl
        pathListsListMap  + (key -> pathListBufferLists)
    }
  }

  override protected def stage = "Path identification"

  override protected def amPair = AmPair(idxListsMap.size, solSubActorAm)


  override def startNewSlave(key: Int){
    val solSubActor = new SolutionSubActor[Decision](this, key, idxListsMap(key))
    link(solSubActor)
    solSubActor.start
  }


  override def getComputedResult: ListBuffer[PathEntry[Decision]] = {
    //MERGE the results
    for(key <- pathListsListMap .keys.toList.sorted){
      var break = false //both for-loops can't be merged because of the "break"
      for(innerList <- pathListsListMap (key) if !break)
        if(pathList.last.currCell == innerList.head.currCell){//the method "==" is defined in the case class "Idx"
          pathList ++= innerList.drop(1)//the first element of the "list" must be dropped to avoid a redundancy
          break = true
        }
    }

    pathList
  }
}
