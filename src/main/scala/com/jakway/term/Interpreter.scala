package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

object Interpreter {
  type SymbolTable = Map[String, Term]


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

/**
  *
  * @param n the interpreter takes an instance of the numeric type
  *          used to do actual calculations
  * @tparam N
  * @tparam M
  */
class Interpreter[N <: NumericType[M], M](n: NumericType[M]) {
  import Interpreter._

  def eval(table: SymbolTable)(t: Term): Either[SimError, Term] = t match {
    case Literal(x) => n.readLiteral(x).map(Raw.apply)

    case v@Variable(name, _) => table.get(name) match {
      case Some(t) => eval(table)(t)
      case None => Left(new SymbolNotFoundError(v, table, t))
    }

    case _ => ???
  }

  /**
    * type for values we've looked up in the current evaluation context
    * @param value
    */
  case class Raw(value: M) extends NumericTerm[N, M] {
    override def contains(t: Term): Boolean = equals(t)
  }
}

