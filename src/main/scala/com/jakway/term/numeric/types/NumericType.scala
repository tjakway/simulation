package com.jakway.term.numeric.types

import com.jakway.term.numeric.{NumericBinaryInvertibleFunction, NumericUnaryInvertibleFunction}

trait NumericType[A] {
  type TrigFunction = NumericUnaryInvertibleFunction[NumericType[A], A]
  val sin: TrigFunction
  val cos: TrigFunction
  val tan: TrigFunction

  val arcsin: TrigFunction
  val arccos: TrigFunction
  val arctan: TrigFunction

  type BinaryMathFunction = NumericBinaryInvertibleFunction[NumericType[A], A]
  val pow: BinaryMathFunction
  val root: BinaryMathFunction
}
