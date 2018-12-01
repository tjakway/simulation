package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.elements._
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.Solvable

/**
  * variables and term are fields and not constructor parameters
  * so that other fields can be used in their definition
  * (allowing you to define variables inside the class
  * that can be referenced in term)
  * @tparam N
  * @tparam M
  */
abstract class Expression[N <: NumericType[M], M] {
  val variables: Seq[Variable[N, M]]
  val term: Term
}

abstract class SolverTestCase[N <: NumericType[M], M] {

  val input: Solvable
  val expected: Solvable

  lazy val variables: Seq[Variable[N, M]] =
    expected.sides.flatMap(TermOperations.findVariables[N, M])

  lazy val expressions: Seq[Expression[N, M]] =
    expected.sides.map { side =>
      new Expression[N, M] {
        override val variables: Seq[Variable[N, M]] =
          TermOperations.findVariables(side)
        override val term: Term = side
      }
    }

  val solveFor: Variable[N, M]

  val namePrefix: String = "SolverTestCase"
  def fullName: String = namePrefix + "." + name
  val name: String
}

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
