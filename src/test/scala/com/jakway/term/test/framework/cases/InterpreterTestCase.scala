package com.jakway.term.test.framework.cases

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.cases.InterpreterTestCase.{BigDecimalTestCase, DoubleTestCase, Expectation}

object InterpreterTestCase {
  abstract class Expectation {
    def input: SymbolTable
    def expectedOutput: Term
  }

  abstract class DomainTestCase[N <: NumericType[M], M]

  class DoubleTestCase[N <: NumericType[Double]]
    extends DomainTestCase[N, Double]

  class BigDecimalTestCase[N <: NumericType[BigDecimal]]
    extends DomainTestCase[N, BigDecimal]
}

abstract class InterpreterTestCase extends NamedTestCase {
  def genericTestCase: Option[Expectation] = None

  def doubleTestCase[N <: NumericType[Double]]: Option[DoubleTestCase[N]] = None
  def doubleTestCaseName: String => String = _ + ".double"

  def bigDecimalTestCase[N <: NumericType[Double]]:
    Option[BigDecimalTestCase[N]] = None
  def bigDecimalTestCaseName: String => String = _ + ".bigdecimal"
}
