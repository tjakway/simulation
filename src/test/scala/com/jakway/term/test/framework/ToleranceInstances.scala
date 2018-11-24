package com.jakway.term.test.framework

import org.scalactic.{Equality, TolerantNumerics}
import java.math.BigDecimal

trait ToleranceInstances {

  val doubleEquality: Equality[Double]
  val intEquality: Equality[Int]
  val bigDecimalEquality: Equality[BigDecimal]
}

object ToleranceInstances {

  class ToleranceInstancesError(override val msg: String)
    extends TestError(msg) {
    def this(t: Throwable) {
      this(s"$t")
    }
  }

  object ToleranceInstancesError {
    def apply(t: Throwable): ToleranceInstancesError =
      new ToleranceInstancesError(t)
  }

  case class NegativeToleranceError(override val msg: String)
    extends ToleranceInstancesError(msg)

  def get(tolerance: String): Either[TestError, ToleranceInstances] = {
    def wrap[A](a: A): Either[TestError, A] =
      TestError.wrapEx[A](ToleranceInstancesError.apply)(a)

    for {
      doubleEq <- wrap(tolerance.toDouble).map(doubleInstance)
      intEq <- intInstance(tolerance)
      bigDecimalEq <- wrap(new BigDecimal(tolerance)).map(new BigDecimalEquality(_))
    } yield {
      new ToleranceInstances {
        override val doubleEquality: Equality[Double] = doubleEq
        override val intEquality: Equality[Int] = intEq
        override val bigDecimalEquality: Equality[BigDecimal] = bigDecimalEq
      }
    }
  }

  def getOrThrow(tolerance: String): ToleranceInstances = {
    get(tolerance) match {
      case Right(x) => x
      case Left(t) => throw ToleranceInstancesError(t)
    }
  }

  def obviousEqualityInstance[A]: Equality[A] = {
    new Equality[A] {
      override def areEqual(a: A, b: Any) = {
        b.isInstanceOf[A] && (a == b.asInstanceOf[A])
      }
    }
  }

  private def doubleInstance(tolerance: Double): Equality[Double] = {
    //if the tolerance is 0, return a straightforward Equality instance
    //that just compares them
    //can't pass it to TolerantNumerics or it will throw an exception
    if (tolerance == 0) {
      obviousEqualityInstance[Double]
    } else {
      TolerantNumerics.tolerantDoubleEquality(tolerance)
    }
  }


  private def intInstance(tolerance: String): Either[TestError, Equality[Int]] = {
    def toleranceDoubleE =
      TestError.wrapEx(ToleranceInstancesError.apply)(tolerance.toDouble)

    toleranceDoubleE.flatMap { toleranceDouble =>
      val zeroToleranceInstance: Either[TestError, Equality[Int]] =
        Right(obviousEqualityInstance[Int])
      if (tolerance == 0) {
        zeroToleranceInstance
      }
      else if (toleranceDouble < 0) {
        Left(NegativeToleranceError(
          s"Cannot have a negative tolerance: $tolerance"))
      } else {
        val res: Either[TestError, Int] =
          TestError.wrapEx[Int](ToleranceInstancesError.apply) {
            java.lang.Math.floor(toleranceDouble).toInt
          }

        res.map(intTolerance =>
          TolerantNumerics.tolerantIntEquality(intTolerance))
      }
    }
  }

  /**
    * Compares instances of BigDecimal within the passed tolerance
    * @param tolerance
    */
  class BigDecimalEquality(val tolerance: BigDecimal)
    extends Equality[BigDecimal] {

    override def areEqual(a: BigDecimal, b: Any): Boolean = {
      def assertCompareTo(i: Int): Unit = {
        assert(i == -1 || i == 0 || i == 1)
      }

      def eq(x: BigDecimal, y: BigDecimal): Boolean = {
        val upper = y.add(tolerance)
        val lower = y.subtract(tolerance)

        val cmpUpper = a.compareTo(upper)
        val cmpLower = a.compareTo(lower)
        //sanity check compareTo results
        assertCompareTo(cmpUpper)
        assertCompareTo(cmpLower)

        //x must be <= upper and >= lower
        (cmpUpper  == -1 || cmpUpper == 0) &&
          (cmpLower == -1 || cmpLower == 0)
      }

      b.isInstanceOf[BigDecimal] &&
        eq(a, b.asInstanceOf[BigDecimal])
    }
  }
}

object CompareNumbers {
  /**
    * returns the result of calling left.compareTo(right)
    * ought to work even for values that are very large or very small or differ in precision
    * @param a
    * @param b
    * @tparam A
    * @tparam B
    * @return
    */
  def compare[A, B](a: A, b: B): Int = {
    val res = new BigDecimal(a.toString).compareTo(new BigDecimal(b.toString))
    //check range of allowable compareTo values
    assert(res == -1 || res == 0 || res == 1)
    res
  }

  def apply[A, B] = compare[A, B] _
}