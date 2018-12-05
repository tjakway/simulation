package com.jakway.term.test.framework.cases

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.cases.InterpreterTestCase.{BigDecimalTestCase, DoubleTestCase, Expectation}

object InterpreterTestCase {
  abstract class Expectation {
    def symbolTable: SymbolTable = Map()
    def input: Term
    def expectedOutput: Term
  }

  abstract class DomainTestCase[N <: NumericType[M], M]
    extends Expectation

  abstract class DoubleTestCase[N <: NumericType[Double]]
    extends DomainTestCase[N, Double]

  abstract class BigDecimalTestCase[N <: NumericType[BigDecimal]]
    extends DomainTestCase[N, BigDecimal]
}

abstract class InterpreterTestCase extends NamedTestCase {
  def genericTestCase: Option[Expectation] = None

  def doubleTestCase[N <: NumericType[Double]]: Option[DoubleTestCase[N]] = None
  def mkDoubleTestCaseName: String => String = _ + ".double"
  def doubleTestCaseName: String = mkDoubleTestCaseName(fullName)

  def bigDecimalTestCase[N <: NumericType[BigDecimal]]:
    Option[BigDecimalTestCase[N]] = None
  def mkBigDecimalTestCaseName: String => String = _ + ".bigdecimal"
  def bigDecimalTestCaseName: String = mkBigDecimalTestCaseName(fullName)
}
