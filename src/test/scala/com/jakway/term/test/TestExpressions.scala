package com.jakway.term.test

import com.jakway.term._
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

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals,
    addTwoVariables.term,
    addThreeVariables.term
  )
}
