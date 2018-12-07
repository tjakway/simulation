package com.jakway.term.test.run

import com.jakway.term.run.Combinations
import org.scalatest.{FlatSpec, Matchers}

class TestCombinations
  extends FlatSpec
    with Matchers {

  //example from http://web.mnstate.edu/peil/MDEV102/U1/S7/Cartesian4.htm
  "Combinations.combinations" should
    "calculate A X B for A = {H, T} and B = {1, 2, 3, 4, 5, 6}" in {

    val aKey = "A"
    val bKey = "B"

    val H = "H"
    val T = "T"
    val in = Seq(
      (aKey, Seq(H, T)),
      (bKey, Seq(1, 2, 3, 4, 5, 6))
    ).toMap


    val expected = Stream(
      Map(aKey -> H, bKey -> 1),
      Map(aKey -> H, bKey -> 2),
      Map(aKey -> H, bKey -> 3),
      Map(aKey -> H, bKey -> 4),
      Map(aKey -> H, bKey -> 5),
      Map(aKey -> H, bKey -> 6),
      Map(aKey -> T, bKey -> 1),
      Map(aKey -> T, bKey -> 2),
      Map(aKey -> T, bKey -> 3),
      Map(aKey -> T, bKey -> 4),
      Map(aKey -> T, bKey -> 5),
      Map(aKey -> T, bKey -> 6))

    val actual = Combinations.combinations(in)
    actual shouldEqual expected
  }

}
