package com.jakway.term.test.framework.cases

import com.jakway.term.TermOperations
import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.Solvable
import com.jakway.term.test.framework.Expression

abstract class SolverTestCase[N <: NumericType[M], M]
  extends NamedTestCase {

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
  val name: String
}
