package com.jakway.term.test.framework

import org.scalactic.{Equality, TolerantNumerics}
import java.math.BigDecimal

trait ToleranceInstances {

  val doubleEquality: Equality[Double]
  val intEquality: Equality[Int]
  val bigDecimalEquality: Equality[BigDecimal]
}

object ToleranceInstances {
  case class ToleranceInstancesError(val t: Throwable)
    extends TestError(t)

  def get(tolerance: String): Either[TestError, ToleranceInstances] = {
    def wrap[A](a: A): Either[TestError, A] =
      TestError.wrapEx[A](ToleranceInstancesError.apply)(a)

    for {
      doubleEq <- wrap(tolerance.toDouble).map(doubleInstance)
      intEq <- wrap(tolerance.toInt).map(intInstance)
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
    if(tolerance == 0) {
      obviousEqualityInstance[Double]
    } else {
      TolerantNumerics.tolerantDoubleEquality(tolerance)
    }
  }

  private def intInstance(tolerance: Int): Equality[Int] = {
    if (tolerance == 0) {
      obviousEqualityInstance[Int]
    } else {
      TolerantNumerics.tolerantIntEquality(tolerance)
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

