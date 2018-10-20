package com.jakway.term.test

import com.jakway.term.{Add, Literal, Term, Variable}
import com.jakway.term.numeric.types.NumericType

class Expressions[N <: NumericType[M], M] {
  abstract class Expression {
    val variables: Seq[Variable[N, M]]
    val term: Term
  }

  val addTwoLiterals: Term =
    Add(Literal("5"), Literal("2"))

  val addTwoVariables = new Expression {
    val variables = Seq(Variable("x", None), Variable("y", None))
    val term = Add(variables(0), variables(1))
  }

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals,
    addTwoVariables.term
  )
}
