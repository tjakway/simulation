package com.jakway.term.test

import com.jakway.term.elements.{Equation, Variable}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.Solver
import com.jakway.term.test.framework.TermMatchers
import org.scalatest.{FlatSpec, Matchers}

class TestSolver[N <: NumericType[M], M]
  (val numericType: N)
  extends FlatSpec
  with Matchers
  with TermMatchers
  with NumericTypeTest[N, M] {

  val testExpr = new TestExpressions[N, M]()

  it should "solve z = x^y" in {
    val z = Variable[N, M]("z")
    val eq = Equation(z, testExpr.xPower.term)

    val solver = new Solver[N, M]()
    println(solver.solve(testExpr.xPower.y)(eq))
  }
}
