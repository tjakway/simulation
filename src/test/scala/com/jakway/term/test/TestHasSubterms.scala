package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

object TestHasSubterms {
  val addTwoLiterals: Term =
    Add(Literal("5"), Literal("2"))

  val allExpressions: Seq[Term] = Seq(
    addTwoLiterals
  )

  def id: Term => Term = x => x
}

/**
  * TODO: parameterize for N and M then create a runner that
  * instantiates tests for each instance of NumericType
  */
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

  type M = Double
  type N = NumericType[M]

  val addZeroTest = new MapAllTest(
    description = "let literals be replaced",
    expected = Add(Literal("50"), Literal("20")),
    input = addTwoLiterals,
    function = t => t match {
        case Literal(x) => Literal(x + "0")
        case x => x
      }
  )

  val changeToMultiplyTest = {
    def f(t: Term): Term = t match {
      case Add(x, y) => Multiply(x, y)
      case x => x
    }

    new MapAllTest(
      description = "change add to multiply",
      expected = Multiply(
        addZeroTest
          .expected.asInstanceOf[HasSubterms].subterms(0)
          .asInstanceOf[NumericTerm[N, M]],
        addZeroTest.expected.asInstanceOf[HasSubterms].subterms(1)
          .asInstanceOf[NumericTerm[N, M]]
      ),
      input = addTwoLiterals,
      function = f _ compose(addZeroTest.function)
    )
  }

  "mapAll" should "apply id correctly" in {
    allExpressions.foreach(
      x => TermOperations.mapAll(x)(id) shouldEqual x)
  }
}
