package com.jakway.term.test.run

import com.jakway.term.run.Combinations
import org.scalatest.{FlatSpec, Matchers}

class TestCombinations
  extends FlatSpec
    with Matchers {

  trait CombinationsTest {
    val in: Map[String, Seq[Any]]
    def actual: Stream[Map[String, Any]] =
      Combinations.combinations(in)

    val expected: Stream[Map[String, Any]]
  }

  def runTest(test: CombinationsTest): Unit = {
    //compare ignoring order
    test.actual.length shouldEqual test.expected.length
    test.actual.toSet shouldEqual test.expected.toSet
  }

  class FirstTest extends CombinationsTest {
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
  }

  class SecondTest extends CombinationsTest {
    trait Foo
    object Bar extends Foo
    object Baz extends Foo

    val cValues: Seq[Foo] = Seq(Bar, Baz)

    val cKey = "C"

    private val firstTest = new FirstTest()
    val in = firstTest.in.updated(cKey, cValues)
    val expected = {
      firstTest.expected.map(_.updated(cKey, Bar)) ++
      firstTest.expected.map(_.updated(cKey, Baz))
    }
  }

  //example from http://web.mnstate.edu/peil/MDEV102/U1/S7/Cartesian4.htm
  "Combinations" should
    "calculate A X B for A = {H, T} and B = {1, 2, 3, 4, 5, 6}" in {
    runTest(new FirstTest())
  }

  it should
    "calculate FirstTest X C for C = {Bar, Baz}" in {
    runTest(new SecondTest())
  }

}
