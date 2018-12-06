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

abstract class SimulationRun(inputs: ValueStreams,
                             output: String,
                             toRun: Solvable,
                             computeValues: ComputeValues = new Permute())

