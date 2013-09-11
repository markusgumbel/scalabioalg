package net.gumbix.analysis

import scala.collection.mutable.{ListBuffer, Map}
import scala.actors.Actor
import scala.util.Random
import scala.collection.mutable
import net.gumbix.dynpro.concurrency.Debugger

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 6/14/13
 * Time: 2:50 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */
protected[analysis] class CreatorActor[SeqDT](elements: List[SeqDT], nrOfSeq: Int, len: Long)
extends Actor{
  
  private def startCreators{
    //Anonymous actor
    class SubCreatorActor extends Actor{
      override def act{
        CreatorActor.this ! (0.asInstanceOf[Long] until len).map(_ =>
          Random.shuffle(elements).head.toString
        ).mkString
        //exit
      }
    }

    (0 until nrOfSeq).foreach(_ => new SubCreatorActor().start)
  }


  override def act{
    startCreators

    val sequences = new ListBuffer[String]()
    react{
      case Message.start =>
        val to = sender
        var i = 0
        loopWhile(i < nrOfSeq){
          react{
            case seq: String =>
              sequences += seq
              i += 1
          }
        }andThen to ! sequences

    }
  }
}


protected[analysis] object Message {
  val start = 'startCreator
}


