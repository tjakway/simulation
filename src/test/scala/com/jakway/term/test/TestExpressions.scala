package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.elements._
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.Solvable
import com.jakway.term.test.framework.Expression
import com.jakway.term.test.framework.cases.SolverTestCase


class SolverTestCases[N <: NumericType[M], M] {

  val xPower: SolverTestCase[N, M] = new SolverTestCase[N, M] {
    val x = Variable[N, M]("x")
    val y = Variable[N, M]("y")
    val z = Variable[N, M]("z")

    val solveFor = y
    val input: Solvable = Solvable(z, Power(x, y))
    val expected: Solvable = Solvable(y, Logarithm(x, z))

    val name: String = "xPower"
  }

  val testCases: Seq[SolverTestCase[N, M]] = Seq(
    xPower
  )

  lazy val testCaseExpressions = testCases.flatMap(_.expressions)
}


class TestExpressions[N <: NumericType[M], M]
  extends SolverTestCases[N, M] {

  val addTwoLiterals: Term =
    Add(Literal("5"), Literal("2"))

  val addTwoVariables = new Expression[N, M] {
    val variables = Seq(Variable("x", None), Variable("y", None))
    val term = Add(variables(0), variables(1))
  }

  val addThreeVariables = new Expression[N, M] {
    val variables = Seq(Variable("x", None),
      Variable("y", None),
      Variable("z", None))
    val term = Add(variables(0), Add(variables(1), variables(2)))
  }

  val deeplyNestedVariables = new Expression[N, M] {
    val a = Variable[N, M]("a", None)
    val b = Variable[N, M]("b", None)
    val c = Variable[N, M]("c", None)
    val d = Variable[N, M]("d", None)
    val e = Variable[N, M]("e", None)

    val variables: Seq[Variable[N, M]] = Seq(a, b, c, d, e)
    val term = Add(a,
      Multiply(Add(Negative(b), Multiply(Literal("5"), c)),
        Multiply(Negative(d), Add(e, Literal("2")))))
  }


  lazy val allExpressions: Seq[Expression[N, M]] = Seq(
    addTwoVariables,
    addThreeVariables,
    deeplyNestedVariables
  ) ++ testCaseExpressions

  lazy val allTerms: Seq[Term] = Seq(
    addTwoLiterals,
  ) ++ allExpressions.map(_.term) ++ testCaseExpressions.map(_.term)
}
