package com.jakway.term.test.framework

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Eval.EvalType
import com.jakway.term.interpreter.{Eval, Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.interpreter.helpers.EvalHelpers
import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.test.NumericTypeTest
import com.jakway.term.test.framework.InterpreterTest.ReadLiteralError
import org.scalactic.Equality
import org.scalatest.{FlatSpec, Matchers}

abstract class InterpreterTest[N <: NumericType[M], M]
  (val numericType: N, implicit val equality: Equality[M])
  extends FlatSpec
    with Matchers
    with TermMatchers
    with NumericTypeTest[N, M] {

  import InterpreterTest._

  //for convenience
  def readLiteral(x: String): M = {
    numericType.readLiteral(x) match {
      case Right(y) => y
      case Left(err) =>
        throw ReadLiteralError(s"Error in InterpreterTest.readLiteral: $err")
    }
  }

  val interpreter: Interpreter = {
    val res = EvalHelpers.setupWithReadLiteralStr(numericType.readLiteral,
      interpreter, numericType)
      .map(helpers => new Eval(numericType)(helpers))

    res match {
      case Right(x) => x
      case Left(err) => throw InterpreterSetupError(err)
    }
  }

  val emptyTable: SymbolTable = Map()
  def eval(t: Term): Term =
    interpreter.eval(emptyTable)(t) match {
      case Right(x) => x
      case Left(err) => throw EvalReturnedError(err)
    }


  //definitions for convenience
  val zero = Raw[N, M](numericType.builtinLiterals.zero)
  val one = Raw[N, M](readLiteral("1"))
  val negativeOne = Raw[N, M](numericType.builtinLiterals.negativeOne)
  val pi = Raw[N, M](numericType.builtinLiterals.pi)
  val e = Raw[N, M](numericType.builtinLiterals.e)

  /**
    * implicit Equality[Term] instance so shouldEqual can be used with Terms
    */
  implicit val termEquality: Equality[Term] = TermMatchers.equalityInstance
}

object InterpreterTest {
  case class ReadLiteralError(override val msg: String)
    extends TestError(msg)

  case class InterpreterSetupError(val t: Throwable)
    extends TestError(t)

  case class EvalReturnedError(e: SimError)
    extends TestError(s"eval gave: $e")
}
