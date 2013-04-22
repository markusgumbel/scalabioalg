package net.gumbix.paradynpro.actors

import scala.actors.Actor
import net.gumbix.paradynpro.DependencyCase._
import net.gumbix.dynpro.Idx

/**
 * An algorithm for dynamic programming. It uses internally a two-dimensional
 * matrix to store the previous results.
 * Project name: scabio
 * Date: 4/19/13
 * Time: 4:27 PM
 * @author Patrick Meppe (tapmeppe@gmail.com)
 */

private class MatrixActor(mx: Array[Array[Option[Double]]], initValue: Double, dep: DependencyCase,
                          calcMatrixIndexValue:(Array[Array[Option[Double]]], Idx,
                                                (Array[Idx], Array[Double]) => Array[Double], (Idx, Double) => Unit)
                            => Array[Array[Option[Double]]]
                         ) extends Actor{


  private def startCellActors{
    //get suitable dimensions for the cell actors
    val (numberOfActors, cellActorMatrixLength) = dep match {
      /*
        mx.length =: maximal number of cells in a column
        mx(0).length =: maximal number of cells in a row
         */
      case LEFT_UPLEFT_UP => (mx.length, mx(0).length)
      case UPLEFT_UP_UPRIGHT => (mx(0).length, mx.length)
      case _ => (0,0)
    }

    //start all the cell actors
    //TODO optimize the number of actors
    for(i <- 0 until numberOfActors)
      new CellActor(this, mx, initValue, cellActorMatrixLength, dep, calcMatrixIndexValue).start(i)

  }


  def act{
    startCellActors

    react{
      case msgGetValues(missingValIndexes) =>
        val _values = for(idx <- missingValIndexes) yield mx(idx.i)(idx.j)
        /*
        initValue can't be safely used because the initial value could occur in another cell,
        in addition either way the loop has to be used @ least twice
        => the current structure.
         */
        val msg = if(_values.contains(None)) Messages.symbol(0)
          else{
            val values = for(_v <- _values) yield _v match{ case Some(_val) => _val }
            msgAckGetValues(values)
          }
        sender ! msg

      case msgUpdateMatrix(idx, newValue) =>
        mx(idx.i)(idx.j) = Some(newValue)

      case 'Die =>
    }
  }

}

