package net.gumbix.dynpro.concurrency.actors

import scala.actors.Actor

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 9/11/13
 * Time: 11:30 AM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 *
 *
 * This class has been marginalized because it doesn't have a key role in
 * the Master/Slave- or the Peer to Peer- communication process.
 * Its purpose is to increase the iteration speed of an Iterator
 * (Map, List, ListBuffer, Array, ArrayBuffer).
 * @param len The length of the Iterator.
 * @param block The block of instructions that should be proceed.
 */
class SpeedUpActor(len: Int, block: Int => Unit) extends Actor{
  val range = 200 //200*3/8 = 75
  val loopEnd: Int = len / range

  def runForeach(s: Int, e: Int) = {
    new Actor{
      def act{(s until e).foreach(i => block(i))}
    }.start
  }
  //(0 to 2).foreach(i => print(i)) //-> 012
  //(0 until 2).foreach(i => print(i)) //-> 01


  def act{
    if(loopEnd == 0) runForeach(0, len) //the only block
    else (0 until loopEnd).foreach(i =>{
      val rI = range*i
      val rrI = rI + range

      if(i == loopEnd - 1){
        val rest = len % range

        if(8*rest < 3*range) //=: rest < range*3/8 (i'm just avoiding doubles)
          runForeach(rI, rrI+rest)//the 2 last blocks shall be merged.
        else{
          runForeach(rI, rrI) //the 2nd to the last block.
          runForeach(rrI, rrI+rest) //the last block.
        }
      }else runForeach(rI, rrI) //one of the other blocks
    })
  }
}
