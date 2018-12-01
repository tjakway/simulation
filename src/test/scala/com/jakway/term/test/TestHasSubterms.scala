package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.elements._
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

object TestHasSubterms {
  def id: Term => Term = x => x
}

abstract class TestHasSubterms[N <: NumericType[M], M]
  (override val numericType: N)
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {
  import TestHasSubterms._

  val expr = new TestExpressions[N, M]()

  class MapAllTest(
    val description: String,
    val expected: Term,
    val input: Term,
    val function: Term => Term) {

    "mapAll" should description in {
      TermOperations.mapAll(input)(function) shouldEqual expected
    }
  }

  val addZeroTest = new MapAllTest(
    description = "let literals be replaced",
    expected = Add(Literal("50"), Literal("20")),
    input = expr.addTwoLiterals,
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
      input = expr.addTwoLiterals,
      function = f _ compose(addZeroTest.function)
    )
  }

  "mapAll" should "apply id correctly" in {
    expr.allTerms.foreach(
      x => TermOperations.mapAll(x)(id) shouldEqual x)
  }
}
