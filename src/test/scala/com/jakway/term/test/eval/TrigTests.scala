package com.jakway.term.test.eval

import com.jakway.term.elements._
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.InterpreterTest
import org.scalactic.Equality
import org.scalatest.FlatSpec

trait TrigTests[N <: NumericType[M], M]
  extends InterpreterTest[N,M] { this: FlatSpec =>

  val two = Raw[N, M](readLiteral("2"))
  val four = Raw[N, M](readLiteral("4"))

  "eval" should "sin(0) = 0" in {
    eval(Sin(zero)) shouldEqual zero
  }

  it should "cos(0) = 1" in {
    eval(Cos(zero)) shouldEqual one
  }

  it should "tan(0) = 0" in {
    eval(Tan(zero)) shouldEqual zero
  }

  it should "arcsin(0) = 0" in {
    eval(Arcsin(zero)) shouldEqual zero
  }

  it should "arccos(1) = 0" in {
    eval(Arccos(one)) shouldEqual zero
  }

  it should "arctan(0) = 0" in {
    eval(Arctan(zero)) shouldEqual zero
  }

  it should "sin(pi/2) = 1" in {
    eval(Sin(Divide(pi, two))) shouldEqual one
  }

  it should "cos(pi) = -1" in {
    eval(Cos(pi)) shouldEqual negativeOne
  }

  it should "cos(pi/4) = 1/sqrt(2)" in {
    eval(Cos(Divide(pi, four))) shouldEqual eval(Divide(one, Power.sqrt(Literal("2"))))
  }

  it should "tan(pi/4) = 1" in {
    eval(Tan(Divide(pi, four))) shouldEqual one
  }

  it should "arcsin(1) = pi/2" in {
    eval(Arcsin(one)) shouldEqual eval(Divide(pi, two))
  }

  it should "arccos(-1) = pi" in {
    eval(Arccos(negativeOne)) shouldEqual pi
  }

  it should "arctan(1) = pi/4" in {
    eval(Arctan(one)) shouldEqual eval(Divide(pi, four))
  }
}
