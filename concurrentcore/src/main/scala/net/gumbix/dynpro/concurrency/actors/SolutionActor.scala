package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency.{Debugger, msgException, msgSolDone}
import net.gumbix.dynpro.{Idx, PathEntry}
import scala.collection.mutable.ListBuffer

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
 * @param matrix
 * @param getPathList
 * @tparam Decision
 */
protected[concurrency] final class SolutionActor[Decision]
  (idx: Idx, matrix: Array[Array[Option[Double]]], range: Int,
   getPathList:(Idx, Array[Array[Option[Double]]], (Idx, Idx)=> Boolean) => ListBuffer[PathEntry[Decision]])
  extends Actor with AbsMasterActor{

  private var pathListsListMap = Map[Int, ListBuffer[ListBuffer[PathEntry[Decision]]]]()

  private lazy val pinPointIdxMap: Map[Int, Idx] = {
    //SPLIT the matrix
    //pin point all the start indexes within the matrix and store them in the map
    var (map, newRange) = (Map(0 -> idx), range)

    while(newRange < idx.MAX){
      val (zi, zj) =
        (if(newRange > idx.i) idx.i else newRange, if(newRange > idx.j) idx.j else newRange)

      //The keys start @ 1 since 0 is already taken.
      map += (newRange/range) -> (idx - (zi, zj)) //pinPointIdx = idx - ~newRange
      newRange += range
    }
    
    map
  }


  /**
   *
   * @param o
   * @return
   */
  protected[actors] def getIdxList(o: Idx): ListBuffer[Idx] = {
    val idxList = new ListBuffer[Idx]()
    idxList += o

    if(o.MIN > 0){
      val loopEnd = 1 + idx.i - o.i
      for(z <- 1 until loopEnd){
        idxList += (o +(0, z))
        idxList += (o +(z, 0))
      }
    }else if(o.i > 0){
      val loopEnd = 1 + idx.j - o.j
      for(z <- 1 until loopEnd) idxList += (o +(0, z))
    }else if(o.j > 0){
      val loopEnd = 1 + idx.i - o.i
      for(z <- 1 until loopEnd) idxList += (o +(z, 0))
    }
    //println(this + "\n\t" + o + "\n\t" + idxList)
    idxList
  }


  /**
   *
   * @param cIdx
   * @return
   */
  private def break(startIdx: Idx, cIdx: Idx): Boolean = {
    val limIdx = startIdx - cIdx

    if(cIdx.i == 0 || cIdx.j == 0) true
    else if(limIdx.i == range || limIdx.j == range) true
    else false
  }


  /**
   * @see
   */
  protected[actors] def getPathList(idx: Idx): ListBuffer[PathEntry[Decision]] =
    getPathList(idx, matrix, break)



  override def actReact{
    react{
      case msgException(key, 0) => restartSlMod(key)

      case msgSolDone(key: Int, pathListsList) =>
        congestionControl
        pathListsListMap  +=
          key -> pathListsList.asInstanceOf[ListBuffer[ListBuffer[PathEntry[Decision]]]]
    }
  }

  override protected def eTerms = ETerms("Path identification", "Range", "")

  override protected def getPoolSize =
    PoolSize(
    pinPointIdxMap.size,
    {val size = pinPointIdxMap.size
    size * (1 + (size - 1)*range)}
    )


  override protected def startNewSlMod(key: Int){
    new SolutionSubActor[Decision](this, key, pinPointIdxMap(key)).start
  }


  def getSolution: ListBuffer[PathEntry[Decision]] = {
    preCompute

    //MERGE the results
    //val pathList = calcSol(idx)

    val pathList = new ListBuffer[PathEntry[Decision]]()
    //sort the keys before the iteration
    pathListsListMap.keys.toList.sorted
    for(key <- pathListsListMap.keys){
      if(key == 0) pathList ++= pathListsListMap(key).head
      else{
        var break = false //both for-loops can't be merged because of the "break"
        for(innerList <- pathListsListMap(key) if !break)
          if(pathList.last.currCell == innerList.head.currCell){//the method "==" is defined in the case class "Idx"
            pathList ++= innerList.drop(1)//the first element of the "list" must be dropped to avoid a redundancy
            break = true
          }
      }
    }

    pathList
  }
}
