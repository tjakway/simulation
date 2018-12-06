package com.jakway.term.test

import com.jakway.term.elements._
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

trait TestNullSubterms[N <: NumericType[M], M]
  extends Matchers { this: FlatSpec =>

  val x = Literal[N, M]("2")
  val y = Literal[N, M]("3")

  def testNullSubterms(h: HasSubterms): Unit = {
    h.subterms.filter(_ == null) shouldEqual Seq()
  }

  "Add" should "not have null subterms" in {
    testNullSubterms(Add(x, y))
  }

  "Subtract" should "not have null subterms" in {
    testNullSubterms(Subtract(x, y))
  }

  "Multiply" should "not have null subterms" in {
    testNullSubterms(Multiply(x, y))
  }

  "Divide" should "not have null subterms" in {
    testNullSubterms(Divide(x, y))
  }

}
