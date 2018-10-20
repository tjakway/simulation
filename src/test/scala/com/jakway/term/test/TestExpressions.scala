package com.jakway.term.test

import com.jakway.term.{Add, Literal, Term, Variable}
import com.jakway.term.numeric.types.NumericType

abstract class Expression[N <: NumericType[M], M] {
  val variables: Seq[Variable[N, M]]
  val term: Term
}

class TestExpressions[N <: NumericType[M], M] {
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

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals,
    addTwoVariables.term,
    addThreeVariables.term
  )
}
