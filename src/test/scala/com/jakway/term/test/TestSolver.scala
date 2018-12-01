package com.jakway.term.test

import com.jakway.term.elements._
import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.solver.{Solvable, Solver}
import com.jakway.term.test.framework.TermMatchers
import org.scalatest.{FlatSpec, Matchers}

class TestSolver[N <: NumericType[M], M]
  (val numericType: N)
  extends FlatSpec
  with Matchers
  with TermMatchers
  with NumericTypeTest[N, M] {

  //only used in tests
  import scala.language.reflectiveCalls

  val testExpr = new TestExpressions[N, M]()

  it should "solve z = x^y" in {
    val z = Variable[N, M]("z")
    val eq = Equation(z, testExpr.xPower.term)

    val solver = new Solver[N, M]()

    val res: Either[SimError, Solvable] =
      solver.solve(testExpr.xPower.y)(eq)
    val expected: Solvable = Solvable(testExpr.xPower.y,
      Logarithm(testExpr.xPower.x, z))

    res shouldEqual Right(expected)
  }
}
