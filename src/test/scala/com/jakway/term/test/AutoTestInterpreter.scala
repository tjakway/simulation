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
                     (fallback: Option[Expectation],
                      numericType: N,
                      name: String, tc: Option[Expectation]): Unit = {

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

  def runDoubleTest(tc: InterpreterTestCase): Unit = {
    runTestForType[
      DoubleTypeInst, Double](tc.genericTestCase, doubleTypeInst,
                              tc.doubleTestCaseName,
                              tc.doubleTestCase)
  }


  def runBigDecimalTest(tc: InterpreterTestCase): Unit = {
    runTestForType[
      BigDecimalTypeInst, BigDecimal](
                                  tc.genericTestCase, bigDecimalTypeInst,
                                  tc.bigDecimalTestCaseName,
                                  tc.bigDecimalTestCase)
  }

  def runTest(tc: InterpreterTestCase) = {
    runDoubleTest(tc)
    runBigDecimalTest(tc)
  }

}
