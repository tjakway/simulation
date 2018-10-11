package com.jakway.term.test

import com.jakway.term.{Add, Literal, Term, TermOperations}
import org.scalatest.{FlatSpec, Matchers}

object TestHasSubterms {
  val addTwoLiterals: Term =
    Add(Literal("5"), Literal("2"))

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals
  )

  def id: Term => Term = x => x
}

class TestHasSubterms extends FlatSpec with Matchers {
  import TestHasSubterms._

  "mapAll" should "apply id correctly" in {
    allExpressions.foreach(
      x => TermOperations.mapAll(x)(id) shouldEqual x)
  }

  it should "let literals be replaced" in {
    val expected = Add(Literal("50"), Literal("20"))

    def addAZero(t: Term): Term = t match {
      case Literal(x) => Literal(x + "0")
      case x => x
    }

    TermOperations.mapAll(addTwoLiterals)(addAZero) shouldEqual expected
  }
}
