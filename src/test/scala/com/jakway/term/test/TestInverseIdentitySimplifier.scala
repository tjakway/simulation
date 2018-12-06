package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.elements.{Literal, Negative, NumericTerm, Term}
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.simplifier.InverseIdentitySimplifier
import org.scalatest.{FlatSpec, Matchers}

trait TestInverseIdentitySimplifier[N <: NumericType[M], M]
  extends Matchers { this: FlatSpec =>

  "InverseIdentitySimplifier.OneSubterm" should "simplify -(-1)" in {
    val inner: NumericTerm[N, M] = Literal("1")
    val term: Term = Negative(Negative(inner))
    val expected: Term = inner

    new InverseIdentitySimplifier().simplify(term) shouldEqual Right(expected)
  }

}
