package com.jakway.term.interpreter

import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.{Literal, NumericTerm, Term, Variable}


/**
  * type for values we've looked up in the current evaluation context
  * @param value
  */
case class Raw[N <: NumericType[M], M](value: M) extends NumericTerm[N, M] {
  override def contains(t: Term): Boolean = equals(t)
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

    def eval(table: SymbolTable)(t: Term): Either[SimError, Term] = t match {
      case Literal(x) => n.readLiteral(x).map(Raw.apply)

      case v@Variable(name, _) => table.get(name) match {
        case Some(t) => eval(table)(t)
        case None => Left(new SymbolNotFoundError(v, table, t))
      }

      case _ => ???
    }

  }
