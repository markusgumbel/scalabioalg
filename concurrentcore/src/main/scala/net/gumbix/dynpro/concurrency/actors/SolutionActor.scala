package net.gumbix.dynpro.concurrency.actors

import net.gumbix.dynpro.concurrency.{Messages, MsgException}
import net.gumbix.dynpro.{Idx, PathEntry}
import scala.collection.mutable.{ListBuffer, Map}

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 5/8/13
 * Time: 5:56 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 * This class represents the master actor during the path finding stage
 * @param idx see DynProConfig.scala
 * @param getPath see DynProConfig.scala
 * @param range ...
 * @tparam Decision see DynProConfig.scala
 */
protected[concurrency] final class SolutionActor[Decision](
  idx: Idx,
  getPath:(Idx, (Idx, Idx)=> Boolean) => ListBuffer[PathEntry[Decision]],
  range: Int
)extends AbsMasterActor{

  private val pathListMap = Map[Int, ListBuffer[ListBuffer[PathEntry[Decision]]]]()
  protected[actors] def updatePathListMap(key: Int, list: ListBuffer[ListBuffer[PathEntry[Decision]]]){
    pathListMap += key -> list
  }

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
  private def pinPointIdxs: Map[Int, Idx] = {
    if(ppiMap.isEmpty){
      //SPLIT the matrix
      //pin point all the start indexes within the matrix and store them in the map
      ppiMap += 0 -> idx
      var newRange = range
      while(newRange < idx.MAX){
        val (zi, zj) =
          (if(newRange > idx.i) idx.i else newRange, if(newRange > idx.j) idx.j else newRange)

        //The keys start @ 1 since 0 is already taken.
        ppiMap(newRange/range) = idx - (zi, zj) //pinPointIdx = idx - ~newRange
        newRange += range
      }
    }

    ppiMap
  }

  /**
   *
   * @param key
   * @return
   */
  protected[actors] def getIdxList(key: Int): ListBuffer[Idx] = {
    val o = pinPointIdxs(key)
    val idxList = ListBuffer(o)

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
   * @param startIdx
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
  protected[actors] def getPath(idx: Idx): ListBuffer[PathEntry[Decision]] =  getPath(idx, break)


  override def actReact{
    react{
      case MsgException(e, key, 0) => handleException(e, key, 0)

      case Messages.DONE => congestionControl
    }
  }

  override protected def eTerms = ETerms("Path identification", "Range", "")

  override protected val getPoolSize = {
    val size = pinPointIdxs.size
    PoolSize(size, size * (1 + (size - 1) * range))
  }



  override protected def startNewSlMod(key: Int){
    //the key value doesn't depend on the matrix dimensions.
    //Once set it is fix for the given slave actor.
    new SolutionSubActor[Decision](this, key).start
  }


  override protected def ackStart: ListBuffer[PathEntry[Decision]] = {
    //MERGE the results (the relative path lists)
    val (pathList, sortedKeys) = //sort the keys before the iteration
    (new ListBuffer[PathEntry[Decision]](), pathListMap.keys.toList.sorted)

    sortedKeys.foreach(key =>
      if(key == 0) pathList ++= pathListMap(key).head
      else{
        //both for-loops can't be merged because of the "break" attribute
        var break = false
        for(innerList <- pathListMap(key) if !break)
          if(pathList.last.currCell == innerList.head.currCell){//the method "==" is defined in the case class "Idx"
            pathList ++= innerList.drop(1)//the first element of the "list" must be dropped to avoid a redundancy
            break = true
          }
    })

    //all the relative path lists have been merged to ONE absolute path list
    pathList
  }

}
