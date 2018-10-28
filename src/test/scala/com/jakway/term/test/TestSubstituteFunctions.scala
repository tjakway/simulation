package com.jakway.term.test

import com.jakway.term.{Add, Equation, Literal, Variable}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.SubstituteFunction
import org.scalatest.{FlatSpec, Matchers}

abstract class TestSubstituteFunctions[N <: NumericType[M], M]
  (override val numericType: N)
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {

  "mkSubstituteFunctions" should "simplify a + b" in {
    val x = Variable[N, M]("x", None)
    val left = Add[N, M](x, Variable("y",  None))
    val right = Literal("1")
    val eq = Equation(left, right)


    val res = for {
      functions <- SubstituteFunction.mkSubstituteFunctions(x, left, left.identity)
      application <- SubstituteFunction.applyFunctions(functions, eq)
    } yield {
      application
    }

    res match {
      case Right(x) => () //TODO: assert
      case Left(e) => throw e
    }
  }
}
