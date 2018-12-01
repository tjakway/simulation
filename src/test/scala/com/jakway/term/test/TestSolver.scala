package com.jakway.term.test

import com.jakway.term.elements._
import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.solver.{Solvable, Solver}
import com.jakway.term.test.framework.TermMatchers
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class TestSolver[N <: NumericType[M], M]
  (val numericType: N)
  extends FunSuite
  with Matchers
  with TermMatchers
  with NumericTypeTest[N, M] {

  val testCases: SolverTestCases[N, M] = new SolverTestCases[N, M]()

  testCases.testCases.foreach { thisTestCase =>
    test(thisTestCase.fullName) {
      runTest(thisTestCase)
    }
  }

  def runTest(testCase: SolverTestCase[N, M]) = {
    val solver = new Solver[N, M]()

    val res: Either[SimError, Solvable] =
      solver.solve(testCase.solveFor)(testCase.input)

    res shouldEqual Right(testCase.expected)
  }
}
