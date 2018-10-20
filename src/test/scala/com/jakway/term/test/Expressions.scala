package com.jakway.term.test

import com.jakway.term.{Add, Literal, Term}
import com.jakway.term.numeric.types.NumericType

class Expressions[N <: NumericType[M], M] {
  val addTwoLiterals: Term =
    Add(Literal("5"), Literal("2"))

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals
  )
}
