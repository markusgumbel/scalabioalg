package net.gumbix.analysis

import scala.collection.mutable.{ListBuffer, Map}
import scala.actors.Actor
import scala.util.Random
import scala.collection.mutable

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 2:50 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[analysis] class CreatorActor[SeqDT](elements: List[SeqDT], nrOfSeq: Int, len: Int)
extends Actor{
  
  private def startCreators{
    //Anonymous actor
    class SubCreatorActor(_actor: CreatorActor[SeqDT]) extends Actor{
      override def act{
        val seq = new mutable.StringBuilder
        for(i <- 0 until len)
          seq ++= Random.shuffle(elements).head.toString

        _actor ! seq
      }
    }

    for(j <- 0 until nrOfSeq) new SubCreatorActor(this).start
  }


  override def act{
    startCreators

    val sequences = new ListBuffer[String]()
    react{
      case Message.start =>
        var i = 0
        loopWhile(i < nrOfSeq){
          react{
            case seq:String =>
              sequences += seq
              i += 1
          }
        }
      reply(sequences)
    }
  }
}


protected[analysis] object Message {
  val start = 'startCreator
}


