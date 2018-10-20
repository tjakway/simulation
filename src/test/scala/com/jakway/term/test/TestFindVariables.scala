package com.jakway.term.test

import scala.reflect.runtime.universe._
import com.jakway.term.{TermOperations, Variable}
import com.jakway.term.numeric.types.NumericType
import org.scalatest.{FlatSpec, Matchers}

abstract class TestFindVariables[N <: NumericType[M], M]
  (override val numericType: N)
  //need to pass the TypeTag implicitly to the point of use,
  //see http://engineering.monsanto.com/2015/05/14/implicits-intro/
  (implicit ev: TypeTag[Variable[N, M]])
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {

  val expr = new Expressions[N, M]()

  "findVariables" should "return Seq() for literals" in {
    TermOperations
      .findVariables[N, M](expr.addTwoLiterals) shouldEqual Seq()
  }

  it should "find variables in an unnested expression" in {
    TermOperations
      .findVariables[N, M](expr.addTwoVariables.term) shouldEqual
        expr.addTwoVariables.variables
  }
}
