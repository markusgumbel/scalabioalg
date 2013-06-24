package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor
import net.gumbix.dynpro.concurrency.{MsgRelSolListDone, MsgException}
import net.gumbix.dynpro.{Idx, PathEntry}
import scala.collection.mutable.{ListBuffer, Map}

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
 * @param getIdx
 * @param getMatrix
 * @param getPathList
 * @tparam Decision
 */
protected[concurrency] final class SolutionActor[Decision]
  (getIdx:() => Idx, getMatrix:() => Array[Array[Option[Double]]], range: Int,
   getPathList:(Idx, Array[Array[Option[Double]]], (Idx, Idx)=> Boolean) => ListBuffer[PathEntry[Decision]])
  extends AbsMasterActor(getMatrix){

  private val pathListsListMap = Map[Int, ListBuffer[ListBuffer[PathEntry[Decision]]]]()

  /**
   * This object is used to reduce to computation period
   * of the "pinPointIdxMap" method.
   */
  private val ppiMap: Map[Int, Idx] = Map()
  /**
   * This method pin points all the necessary idx within the matrix
   * Note: Since for one object of this class the matrix can easily change,
   *  meaning this method is will have to be re invoked;
   *  it would be unwise to use "lazy val" instead of "def"
   * @return
   */
  private def pinPointIdxMap: Map[Int, Idx] = if(ppiMap.isEmpty){

    //SPLIT the matrix
    //pin point all the start indexes within the matrix and store them in the map
    ppiMap += 0 -> getIdx()
    var newRange = range
    while(newRange < getIdx().MAX){
      val (zi, zj) =
        (if(newRange > getIdx().i) getIdx().i else newRange, if(newRange > getIdx().j) getIdx().j else newRange)

      //The keys start @ 1 since 0 is already taken.
      ppiMap(newRange/range) = (getIdx() - (zi, zj)) //pinPointIdx = idx - ~newRange
      newRange += range
    }

    ppiMap
  }else ppiMap


  /**
   *
   * @param o
   * @return
   */
  protected[actors] def getIdxList(o: Idx): ListBuffer[Idx] = {
    val idxList = ListBuffer(o)

    if(o.MIN > 0){
      val loopEnd = 1 + getIdx().i - o.i
      for(z <- 1 until loopEnd){
        idxList += (o +(0, z))
        idxList += (o +(z, 0))
      }
    }else if(o.i > 0){
      val loopEnd = 1 + getIdx().j - o.j
      for(z <- 1 until loopEnd) idxList += (o +(0, z))
    }else if(o.j > 0){
      val loopEnd = 1 + getIdx().i - o.i
      for(z <- 1 until loopEnd) idxList += (o +(z, 0))
    }
    //println(this + "\n\t" + o + "\n\t" + idxList)
    idxList
  }


  /**
   * @see
   */
  protected[actors] def getPathList(idx: Idx): ListBuffer[PathEntry[Decision]] = {
    /**
     *
     * @param startIdx
     * @param cIdx
     * @return
     */
    def break(startIdx: Idx, cIdx: Idx): Boolean = {
      val limIdx = startIdx - cIdx

      if(cIdx.i == 0 || cIdx.j == 0) true
      else if(limIdx.i == range || limIdx.j == range) true
      else false
    }

    getPathList(idx, matrix, break)
  }



  override def actReact{
    react{
      case MsgException(e, key, 0) => handleException(e, key, 0)

      case MsgRelSolListDone(key, pathListsList) =>
        congestionControl
        pathListsListMap  +=
          key -> pathListsList.asInstanceOf[ListBuffer[ListBuffer[PathEntry[Decision]]]]
    }
  }

  override protected def eTerms = ETerms("Path identification", "Range", "")

  override protected def getPoolSize = {
    val size = pinPointIdxMap.size
    PoolSize(size, size * (1 + (size - 1) * range))
  }



  override protected def startNewSlMod(key: Int){
    def getPpIdx() = pinPointIdxMap(key)
    val actor = new SolutionSubActor[Decision](this, key, getPpIdx)
    slModules += actor
    actor.start
  }


  override protected def restartSlMod(key: Int){slModules(key).restart}


  override protected def ackStart: ListBuffer[PathEntry[Decision]] = {
    //MERGE the results (the relative path lists)
    val (pathList, sortedKeys) = //sort the keys before the iteration
    (new ListBuffer[PathEntry[Decision]](), pathListsListMap.keys.toList.sorted)

    for(key <- sortedKeys){
      if(key == 0) pathList ++= pathListsListMap(key).head
      else{
        //both for-loops can't be merged because of the "break" attribute
        var break = false
        for(innerList <- pathListsListMap(key) if !break)
          if(pathList.last.currCell == innerList.head.currCell){//the method "==" is defined in the case class "Idx"
            pathList ++= innerList.drop(1)//the first element of the "list" must be dropped to avoid a redundancy
            break = true
          }
      }
    }

    //reset the current object to its initial configuration
    ppiMap.clear
    pathListsListMap.clear

    //all the relative path lists have been merged to ONE absolute path list
    pathList
  }

}
