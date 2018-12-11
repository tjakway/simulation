package com.jakway.term.interpreter

import com.jakway.term._
import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType

object Interpreter {
  type SymbolTable = Map[String, Term]

  val emptySymbolTable: SymbolTable = Map()

  def mergeSymbolTables(x: SymbolTable, y: SymbolTable): SymbolTable = {
    y.toSeq.foldLeft(x) {
      case (acc, thisPair) => acc.updated(thisPair._1, thisPair._2)
    }
  }

  class InterpreterError(override val msg: String,
                         val table: SymbolTable,
                         val term: Term)
    extends SimError(s"Error occurred in interpretation" +
      s" of term < $term > with symbol table < $table >: $msg")

  class SymbolNotFoundError[N <: NumericType[M], M]
                           (val variable: Variable[N, M],
                            override val table: SymbolTable,
                            override val term: Term)
    extends InterpreterError(variable.toString, table, term)
}

trait Interpreter {
  def eval(table: SymbolTable)(t: Term): Either[SimError, Term]
}

/**
  * an optimization wraps an existing interpreter
  * and returns one that performs better
  */
trait Optimization {
  def wrap(i: Interpreter): Interpreter
}