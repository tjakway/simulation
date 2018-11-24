package com.jakway.term.test

import com.jakway.term.test.framework.CompareNumbers
import org.scalatest.{FlatSpec, Matchers}

class TestCompareNumbers
  extends FlatSpec with Matchers {


  val lt: Int = -1
  val eq: Int = 0
  val gt: Int = 1

  "CompareNumbers" should
    "give 0: Int < Double.MinValue.abs" in {
    val zero: Int = 0
    CompareNumbers.apply(zero, Double.MinValue.abs) shouldEqual lt
  }

  it should "give 0: Int == 0: Long" in {
    CompareNumbers.apply(0: Int, 0: Long) shouldEqual eq
  }

  it should "give 0: Double == 0: Long" in {
    CompareNumbers.apply(0: Double, 0: Long) shouldEqual eq
  }

  it should "give Long.MinValue < Int.MinValue" in {
    CompareNumbers.apply(Long.MinValue, Int.MinValue) shouldEqual lt
  }

}
