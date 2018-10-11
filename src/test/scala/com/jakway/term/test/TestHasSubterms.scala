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

  val asserter = this

  class MapAllTest(
    val description: String,
    val expected: Term,
    val input: Term,
    val function: Term => Term) {

    "mapAll" should description in {
      TermOperations.mapAll(input)(function) shouldEqual expected
    }
  }

  new MapAllTest(
    description = "let literals be replaced",
    expected = Add(Literal("50"), Literal("20")),
    input = addTwoLiterals,
    function = t => t match {
        case Literal(x) => Literal(x + "0")
        case x => x
      }
  )

  "mapAll" should "apply id correctly" in {
    allExpressions.foreach(
      x => TermOperations.mapAll(x)(id) shouldEqual x)
  }
}
