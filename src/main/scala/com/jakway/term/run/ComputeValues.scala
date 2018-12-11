package com.jakway.term.run

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.ValueStreams

trait ComputeValues {
  val constants: SymbolTable = Interpreter.emptySymbolTable
  protected def toSymbolTables(values: ValueStreams): Stream[SymbolTable]

  def getSymbolTables(values: ValueStreams): Either[SimError, Stream[SymbolTable]] = {

    //make sure the variables listed in constants aren't also listed in streams
    val intersection = values.keySet.intersect(constants.keySet)
    val keysOverlap: Boolean = values.keySet.intersect(constants.keySet) != Set()
    if(keysOverlap) {
      Left(ComputeValues.VariablesGivenTwice(intersection))
    } else {
      //merge the tables and return
      val streamTables = toSymbolTables(values)
      val mergedTables = streamTables.map(thisTable =>
        Interpreter.mergeSymbolTables(thisTable, constants))
      Right(mergedTables)
    }
  }

}

object ComputeValues {
  case class VariablesGivenTwice(val names: Set[String])
    extends SimError(s"Variables $names are given in " +
      s"both the constants table and the ValueStreams (" +
      s"it may only be specified in one)")
}

class Combinations(
  override val constants: SymbolTable = Interpreter.emptySymbolTable)
  extends ComputeValues {
  import Combinations._

  override protected def toSymbolTables(values: ValueStreams): Stream[SymbolTable] = {
    combinations[String, Term](values)
  }
}

object Combinations {
  def combinations[A, B](in: Map[A, Traversable[B]]): Stream[Map[A, B]] = {
    def helper(thisMap: (A, Traversable[B]),
               remainingMaps: Stream[(A, Traversable[B])]): Stream[Map[A, B]] = {
      val (thisKey, values) = thisMap


      val vStream: Stream[(A, B)] = values.map(v => (thisKey, v))
        .toStream
      vStream.flatMap { (q: (A, B)) =>
        remainingMaps.headOption match {
          case Some(next) => {
            helper(next, remainingMaps.drop(1))
              .map(m => m.updated(q._1, q._2))
          }
          case None => Stream(Map(q))
        }
      }
    }

    val inStream = in.toStream
    inStream.headOption match {
      case Some(x) => helper(x, inStream.drop(1))
      case None => Stream()
    }
  }
}
