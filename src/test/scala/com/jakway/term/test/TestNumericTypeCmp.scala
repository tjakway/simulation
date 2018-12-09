package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.TestNumericTypeCmp.TestNumericTypeCmpError
import com.jakway.term.test.framework.TestError
import com.jakway.term.test.framework.gen.HasNumericType
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.annotation.tailrec

trait TestNumericTypeCmp[N <: NumericType[M], M]
  extends HasNumericType[N, M] {
  this: FlatSpec with Matchers =>

  private val lt: Int = -1
  private val eq: Int = 0
  private val gt: Int = 1

  private val zero = numericType.builtinLiterals.zero
  private val negativeOne = numericType.builtinLiterals.negativeOne
  private lazy val one = numericType.readLiteral("1")
  private def comparator = numericType.comparator

  private def throwIfBadCmp(cmpRet: Int): Int = {
    if(cmpRet == -1 || cmpRet == 0 || cmpRet == 1) {
      cmpRet
    } else {
      throw TestNumericTypeCmpError(
          "numericType.comparator.compare returned " +
            s"$cmpRet (expected -1, 0, 1)")
    }
  }

  private def symmetricCmp(left: M, right: M, expected: Int): Unit = {
    @tailrec
    def helper(l: M, r: M, expected: Int, recurse: Boolean = true): Unit = {
      val nextExpected = expected match {
        case 0 => 0
        case 1 => -1
        case -1 => 1
        case _ => throw TestNumericTypeCmpError(s"" +
          s"expected: Int passed to symmetricCmp was $expected; " +
          s"must be -1, 0, or 1 for less than, equal to, or greater" +
          s" than respectively")
      }

      val res = throwIfBadCmp(numericType.comparator.compare(l, r))
      res shouldEqual expected

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


  "NumericType.comparator" should
    "compare 0 and -1 (from builtinLiterals)" in {
    symmetricCmp(zero, negativeOne, gt)
  }

  it should "compare 0 and 1" in {
    one.map(o => symmetricCmp(zero, o, lt)) should be ('right)
  }
}

object TestNumericTypeCmp {
  case class TestNumericTypeCmpError(override val msg: String)
    extends TestError(msg)
}
