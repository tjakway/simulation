package com.jakway.term.interpreter

import com.jakway.term.elements._
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}

import Eval._

/**
  * type for values we've looked up in the current evaluation context
  * @param value
  */
case class Raw[N <: NumericType[M], M](value: M) extends NumericTerm[N, M] {
  override def contains(t: Term): Boolean = equals(t)

  override def matches(other: Term): Boolean = {
    sameType(other) && value == other.asInstanceOf[Raw[N, M]].value
  }
}

/**
  *
  * @param n the interpreter takes an instance of the numeric type
  *          used to do actual calculations
  * @tparam N
  * @tparam M
  */
class Eval[N <: NumericType[M], M](n: NumericType[M])
  extends Interpreter {

  import Interpreter._

  def eval(table: SymbolTable)(t: Term): EvalType = t match {
    case Literal(x) => n.readLiteral(x).map(Raw.apply)

    case v@Variable(name, _) => table.get(name) match {
      case Some(t) => eval(table)(t)
      case None => Left(new SymbolNotFoundError(v, table, t))
    }

    case f: FunctionCall[N, M] => EvalFunctionCall.evalFunctionCall[N, M](table)(this, f)

    case _ => ???
  }
}

object Eval {
  type EvalType = Either[SimError, Term]

  class EvalError(override val msg: String)
    extends SimError(msg)

  def lookupTerm[N <: NumericType[M], M]
            (table: SymbolTable, variable: Variable[N, M]): Option[Term] =
    table.get(variable.name)
}

object EvalFunctionCall {
  case class WrongNumberOfArgumentsError(override val msg: String)
    extends EvalError(msg)

  def evalFunctionCall[N <: NumericType[M], M](table: SymbolTable)
                                              (recurse: Interpreter,
                                               f: FunctionCall[N, M]): EvalType
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
