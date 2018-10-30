package com.jakway.term.interpreter

import com.jakway.term.elements._
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}
import Eval._
import com.jakway.term.interpreter.helpers.{EvalFunctionCall, EvalHelpers}

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

    case x => Left(NotImplementedError(x))
  }
}

object Eval {
  type EvalType = Either[SimError, Term]

  class EvalError(override val msg: String)
    extends SimError(msg)

  case class NotImplementedError(t: Term)
    extends EvalError(s"eval not implemented for term $t")

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
