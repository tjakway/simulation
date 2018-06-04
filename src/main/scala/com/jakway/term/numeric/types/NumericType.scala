package com.jakway.term.numeric.types

import com.jakway.term.{Literal, Term}

class SimError(val msg: String)
  extends RuntimeException(msg)

trait NumericType[M] {
  type UnaryFunction = M => Either[SimError, M]
  type TrigFunction = UnaryFunction

  val sin: TrigFunction
  val cos: TrigFunction
  val tan: TrigFunction

  val arcsin: TrigFunction
  val arccos: TrigFunction
  val arctan: TrigFunction

  type BinaryMathFunction = M => M => Either[SimError, M]

  val pow: BinaryMathFunction
  val root: BinaryMathFunction

  val plus: BinaryMathFunction
  val times: BinaryMathFunction
  val div: BinaryMathFunction

  val readLiteral: String => Either[SimError, M]
}

trait NumericTypeImplementation[M] extends NumericType[M] {
  /**
    * helper method for functions that can't fail
    * @param f
    * @return
    */
  def total(f: M => M): M => Either[SimError, M] = (x: M) => Right(f(x))
  def total(f: M => M => M): M => M => Either[SimError, M] =
    (x: M) => (y: M) => Right(f(x)(y))
}

/**
  *
  * @param n the interpreter takes an instance of the numeric type
  *          used to do actual calculations
  * @tparam N
  * @tparam M
  */
class Interpreter[N <: NumericType[M], M](n: NumericType[M]) {
  def eval(t: Term) = t match {

  }
}