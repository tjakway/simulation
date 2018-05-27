package com.jakway.term

import scala.{math => M}
import com.jakway.term.NumericTypeFactory.TrigFunctions

trait Term

trait NumericTerm[N <: NumericType[M], M]

case class Literal[N <: NumericType[M], M](value: M)
  extends NumericTerm[N, M]

case class Variable[N <: NumericType[M], M](name: String, description: String)
  extends NumericTerm[N, M]

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]


class NumericFunctionApplication[N <: NumericType[M], M, F](
  function: Function[N, M, F], args: Seq[NumericTerm[N, M]])
  extends NumericTerm[N, M]

case class SingleArgFunctionApplication[N <: NumericType[M], M, F](
  function: Function[N, M, F], arg: NumericTerm[N, M])
  extends NumericFunctionApplication[N, M, F](function, Seq(arg))

case class DoubleArgFunctionApplication[N <: NumericType[M], M, F](
  function: Function[N, M, F],
  first: NumericTerm[N, M], second: NumericTerm[N, M])
  extends NumericFunctionApplication[N, M, F](function, Seq(first, second))


//equals is NOT a term--it's an equation
case class Equals(left: Term, right: Term)