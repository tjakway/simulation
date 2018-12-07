package com.jakway.term.run

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.run.SimulationRun.ValueStreams

trait ComputeValues {
  def toSymbolTables(values: ValueStreams): Stream[SymbolTable]
}

class Combinations extends ComputeValues {
  import Combinations._

  def toSymbolTables(values: ValueStreams): Stream[SymbolTable] = {
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
