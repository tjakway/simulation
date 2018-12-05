package com.jakway.term.test

import com.jakway.term.interpreter.Eval
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.cases.InterpreterTestCase
import com.jakway.term.test.framework.cases.InterpreterTestCase.Expectation
import org.scalatest.{FunSuite, Matchers}

class AutoTestInterpreter[DoubleTypeInst <: NumericType[Double],
                          BigDecimalTypeInst <: NumericType[BigDecimal]]
  (val doubleTypeInst: DoubleTypeInst,
   val bigDecimalTypeInst: BigDecimalTypeInst)
  extends FunSuite
  with Matchers {

  def runTestForType[N <: NumericType[M], M]
                     (fallback: Option[Expectation])
                     (numericType: N)
                     (name: String, tc: Option[Expectation]) = {

    test(name) {
      val optTc: Option[Expectation] = tc match {
        case x@Some(_) => x
        case None => fallback
      }

      optTc match {
        case Some(thisTc) => {
          val interpreter = Eval.apply[N, M](numericType)
          interpreter.map(
            _.eval(thisTc.symbolTable)(thisTc.input)) shouldEqual Right(thisTc.expectedOutput)
        }
        case None => {}
      }

    }
  }

  def runTest(tc: InterpreterTestCase) = {
    test(tc.doubleTestCaseName) {
      //use the generic test case if there isn't one specified for this type
      val optTc: Option[Expectation] = tc.doubleTestCase match {
        case x@Some(_) => x
        case None => tc.genericTestCase
      }

      optTc match {
        case Some(thisTc) => {
          val interpreter = Eval.apply[DoubleTypeInst, Double](doubleTypeInst)
          interpreter.map(
            _.eval(thisTc.symbolTable)(thisTc.input)) shouldEqual Right(thisTc.expectedOutput)
        }
        case None => {}
      }
    }

  }

}
