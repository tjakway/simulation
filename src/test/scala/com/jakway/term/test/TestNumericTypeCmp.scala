package com.jakway.term.test

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.TestNumericTypeCmp.TestNumericTypeCmpError
import com.jakway.term.test.framework.TestError
import com.jakway.term.test.framework.gen.{BasePropertiesTrait, HasNumericType}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec


trait SymmetricCmp[N <: NumericType[M], M]
  extends HasNumericType[N, M] {

  def throwIfBadCmp(cmpRet: Int): Int = {
    if(cmpRet == -1 || cmpRet == 0 || cmpRet == 1) {
      cmpRet
    } else {
      throw TestNumericTypeCmpError(
        "numericType.comparator.compare returned " +
          s"$cmpRet (expected -1, 0, 1)")
    }
  }

  def symmetricCmp(doAssert: Int => Int => Unit)
                  (left: M, right: M, expected: Int): Unit = {
    @tailrec
    def helper(l: M, r: M, hExpected: Int, recurse: Boolean = true): Unit = {
      val nextExpected = hExpected match {
        case 0 => 0
        case 1 => -1
        case -1 => 1
        case _ => throw TestNumericTypeCmpError(s"" +
          s"expected: Int passed to symmetricCmp was $hExpected; " +
          s"must be -1, 0, or 1 for less than, equal to, or greater" +
          s" than respectively")
      }

      val res = throwIfBadCmp(numericType.comparator.compare(l, r))
      doAssert(res)(hExpected)

      if(recurse) {
        helper(r, l, nextExpected, false)
      } else {}
    }

    //anything compared to itself should be 0
    def selfCmp(x: M): Unit = helper(x, x, 0, false)

    selfCmp(left)
    selfCmp(right)
    helper(left, right, expected, true)
  }
}

object SymmetricCmp {
  val lt: Int = -1
  val eq: Int = 0
  val gt: Int = 1
}

trait TestNumericTypeCmp[N <: NumericType[M], M]
  extends HasNumericType[N, M]
    with SymmetricCmp[N, M] {
  this: FlatSpec with Matchers =>
  import SymmetricCmp._

  private def zero = numericType.builtinLiterals.zero
  private def negativeOne = numericType.builtinLiterals.negativeOne
  private lazy val one = numericType.readLiteral("1")
  private def comparator = numericType.comparator

  def fSymmetricCmp =
    symmetricCmp(a => b => a shouldEqual b) _


  "NumericType.comparator" should
    "compare 0 and -1 (from builtinLiterals)" in {
    fSymmetricCmp(zero, negativeOne, gt)
  }

  it should "compare 0 and 1" in {
    one.map(o => fSymmetricCmp(zero, o, lt)) should be ('right)
  }

  it should "compare 0 and 9.008601144797282E307" in {
    numericType.readLiteral("9.008601144797282E307").map {
      gtZero =>
        fSymmetricCmp(gtZero, zero, gt)
    } should be ('right)
  }
}

object TestNumericTypeCmp {
  case class TestNumericTypeCmpError(override val msg: String)
    extends TestError(msg)

}

trait NumericTypeCmpProperties[N <: NumericType[M], M]
  extends HasNumericType[N, M]
    with BasePropertiesTrait
    with SymmetricCmp[N, M] { this: Properties =>

  import SymmetricCmp._
  import org.scalacheck.Prop.forAll

  def assertReadLiteral(lit: String): M =
    numericType.readLiteral(lit) match {
      case Right(x) => x
      case Left(err) =>
        throw NumericTypeCmpProperties.ReadLiteralError(err)
    }

  private def zero = numericType.builtinLiterals.zero
  private def negativeOne = numericType.builtinLiterals.negativeOne

  case class GtZeroM(val m: M)
  implicit val arbitraryGtZero: Arbitrary[GtZeroM] =
    Arbitrary(
      Gen.chooseNum(0: Double, NumericType.AllTypes.smallestMaxOfAllTypes.toDouble)
        .filter(_ > 0) //chooseNum is inclusive
        .map(x => GtZeroM(assertReadLiteral(x.toString))))

  property("cmp(gtZero)") = forAll { (gtZero: GtZeroM) =>
    var isEq: Boolean = false

    def setIsEq = (x: Int) => (y: Int) => {
      isEq = (x == y)
      if(!isEq) {
        logger.warn(s"setIsEq: x != y (x=$x, y=$y)")
      }
    }

    symmetricCmp(setIsEq)(gtZero.m, zero, gt)

    isEq
  }
}

object NumericTypeCmpProperties {
  case class ReadLiteralError(e: SimError)
    extends TestError(e)
}
