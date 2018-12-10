package com.jakway.term.test.framework

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.interpreter.{Eval, Interpreter, Raw}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.NumericTypeTest
import org.scalactic.Equality
import org.scalatest.{FlatSpec, Matchers}

trait InterpreterTest[N <: NumericType[M], M]
    extends Matchers
    with NumericTypeTest[N, M] { this: FlatSpec =>

  import InterpreterTest._
  implicit val equality: Equality[M]

  //for convenience
  def readLiteral(x: String): M = {
    numericType.readLiteral(x) match {
      case Right(y) => y
      case Left(err) =>
        throw ReadLiteralError(s"Error in InterpreterTest.readLiteral: $err")
    }
  }

  val interpreter: Interpreter = Eval[N, M](numericType).right.get

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
  implicit val termEqualityObj: Equality[Term] =
    new NumericTermComp(TermMatchers.equalityInstance)

  /**
    * if the terms aren't equivalent, try comparing any underlying
    * numbers using the Equality[M] object
    * @param termEquality used to compare the terms
    */
  class NumericTermComp(val termEquality: Equality[Term]) extends Equality[Term] {
    override def areEqual(a: Term, b: Any): Boolean = {
      termEquality.areEqual(a, b) ||
        (b.isInstanceOf[Term] &&
          cmpNumericValues(a, b.asInstanceOf[Term]).getOrElse(false))
    }

    /**
      * if the terms are both Raw, compare them using
      * the Equality[M] object
      * @param a
      * @param b
      * @return
      */
    def cmpNumericValues(a: Term, b: Term): Option[Boolean] = {
      //we intentionally don't extract or read literals here since the
      //interpreter should be doing that
      def getRaw(x: Term): Option[M] = x match {
        case y: Raw[N @unchecked, M @unchecked] => Some(y.value)
        case _ => None
      }

      for {
        x <- getRaw(a)
        y <- getRaw(b)
      } yield {
        equality.areEquivalent(x, y)
      }
    }
  }
}

object InterpreterTest {
  case class ReadLiteralError(override val msg: String)
    extends TestError(msg)

  case class EvalReturnedError(e: SimError)
    extends TestError(s"eval gave: $e")
}
