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
class Eval[N <: NumericType[M], M](val n: NumericType[M])
                                  (val evalHelpers: EvalHelpers[N, M])
  extends Interpreter {
  import Interpreter._

  def readLiteral(l: Literal[N, M]): Either[SimError, Raw[N, M]] = l match {
    case Literal(value) => n.readLiteral(value).map(Raw.apply)
  }


  def eval(table: SymbolTable)(t: Term): EvalType = t match {
    case l: Literal[N, M] => readLiteral(l)

    case v@Variable(name, _) => table.get(name) match {
      case Some(t) => eval(table)(t)
      case None => Left(new SymbolNotFoundError(v, table, t))
    }

    case Negative(arg) => eval(table)(Multiply(arg, Literal("-1")))

    case f: FunctionCall[N, M] => evalHelpers.functionCall(table)(f)

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

  abstract class EvalHelper[A <: Term](val interpreter: Interpreter)
  {
    def apply(table: SymbolTable)(a: A): EvalType

    def recurse(table: SymbolTable)(t: Term): EvalType =
      interpreter.eval(table)(t)
  }
}

class EvalHelpers[N <: NumericType[M], M](
     val negativeTerm: EvalNegative[N, M],
     val functionCall: EvalFunctionCall[N, M]
)

object EvalHelpers {
  def setup[N <: NumericType[M], M](
           readLiteral: Literal[N, M] => Either[SimError, Raw[N, M]],
           recurse: Interpreter,
           table: SymbolTable): Either[SimError, EvalHelpers[N, M]] = {
    for {
      negativeOne <- readLiteral(Literal[N, M]("-1"))
    } yield {
      new EvalHelpers[N, M](
        new EvalNegative[N, M](recurse, negativeOne),
        new EvalFunctionCall[N, M](recurse))
    }
  }
}


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

/**
  * replace Negative(x) with (x * -1) and eval
  * @param negativeOne
  * @tparam N
  * @tparam M
  */
class EvalNegative[N <: NumericType[M], M](val recurse: Interpreter,
                                           val negativeOne: Raw[N, M])
  extends EvalHelper[Negative[N, M]](recurse) {

  def replaceWith(neg: Negative[N, M]): Term =
    Multiply(neg.arg, negativeOne)

  def apply(t: SymbolTable)
           (term: Negative[N, M]): EvalType =
    recurse(t)(replaceWith(term))
}
