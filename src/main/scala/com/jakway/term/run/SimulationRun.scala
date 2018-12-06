package com.jakway.term.run

import com.jakway.term.Util
import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.interpreter.Interpreter
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.run.SimulationRun.ValueStreams
import com.jakway.term.solver.Solvable

object SimulationRun {
  type ValueStreams = Map[String, Stream[Term]]
}

abstract class SimulationRun
                            (inputs: ValueStreams,
                             output: String,
                             toRun: Solvable)


abstract class ComputeValues(val values: ValueStreams) {

  def toSymbolTables(): Stream[SymbolTable]
}

class Permute(val values: ValueStreams) {
  import Permute._

  def toSymbolTables(): Stream[SymbolTable] = {
    cartesianProduct[String, Term](values)
  }
}

object Permute {
  def cartesianProduct[A, B](in: Map[A, Traversable[B]]): Stream[Map[A, B]] = {
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

