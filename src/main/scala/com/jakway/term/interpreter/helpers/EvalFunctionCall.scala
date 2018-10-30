package com.jakway.term.interpreter.helpers

import com.jakway.term.elements.FunctionCall
import com.jakway.term.interpreter.Eval.{EvalError, EvalHelper, EvalType}
import com.jakway.term.interpreter.Interpreter
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType

class EvalFunctionCall[N <: NumericType[M], M]
  (val recurse: Interpreter)
  extends EvalHelper[FunctionCall[N, M]](recurse) {

  case class WrongNumberOfArgumentsError(override val msg: String)
    extends EvalError(msg)

  def apply(table: SymbolTable)
           (f: FunctionCall[N, M]): EvalType
    = f match {
    case FunctionCall(function, args)
      if function.arity != args.length =>
      Left(WrongNumberOfArgumentsError(s"Function $function takes " +
        s"${function.arity} arguments, got ${args.length}: $args"))

    case FunctionCall(function, args)
      if function.arity == args.length => {

      //associate arguments with their names based on position
      //e.g. for f(x, y) the first argument gets named "x"
      val newSymbols = function.parameters
                              .map(_.name)
                              .zip(args)

      //update the symbol table with function argument names
      val newSymbolTable: SymbolTable = newSymbols.foldLeft(table) {
        case (updatingTable, (thisArgName, thisArg)) =>
          updatingTable.updated(thisArgName, thisArg)
      }

      //eval the function body
      recurse.eval(newSymbolTable)(function.body)
    }
  }
}
