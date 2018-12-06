package com.jakway.term.test.framework

import org.scalactic.Equality
import java.math.BigDecimal

import scala.util.Try

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

  def getToDecimalPlaces(numPlaces: Int):
    Either[TestError, ToleranceInstances] = {

    val zeros: Option[String] =
      if(numPlaces <= 1) {
        None
      } else {
        val stringBuilder = new StringBuilder()
        (0 to (numPlaces - 1))
          .foreach(_ => stringBuilder.append("0"))
        Some(stringBuilder.toString())
      }

    val tolerance: String = "0" + zeros.map("." + _ + "1").getOrElse("")
    if(numPlaces < 0) {
      Left(NegativeToleranceError(
        "Cannot have a negative number of decimal places"
        + s"(passed $numPlaces)"))
    } else {
      get(tolerance)
    }
  }

  def get(tolerance: String): Either[TestError, ToleranceInstances] = {
    def wrap[A](a: A): Either[TestError, A] =
      TestError.wrapEx[A](ToleranceInstancesError.apply)(a)

    for {
      doubleEq <- wrap(tolerance.toDouble).map(new DoubleEquality(_))
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

  private def toleranceDoubleE(tolerance: String) =
    TestError.wrapEx(ToleranceInstancesError.apply)(tolerance.toDouble)

  private def doubleInstance(tolerance: String): Either[TestError, Equality[Double]] = {
    toleranceDoubleE(tolerance).flatMap { toleranceDouble =>
      if (toleranceDouble < 0) {
        Left(NegativeToleranceError(
          s"Cannot have a negative tolerance: $tolerance"))
      } else {
          Right(new DoubleEquality(tolerance.toDouble))
      }
    }
  }

  private def intInstance(tolerance: String): Either[TestError, Equality[Int]] = {
    toleranceDoubleE(tolerance).flatMap { toleranceDouble =>
      if (toleranceDouble < 0) {
        Left(NegativeToleranceError(
          s"Cannot have a negative tolerance: $tolerance"))
      } else {
        val res: Either[TestError, Int] =
          TestError.wrapEx[Int](ToleranceInstancesError.apply) {
            java.lang.Math.floor(toleranceDouble).toInt
          }

        res.map(new IntEquality(_))
      }
    }
  }

  /**
    * Compares numbers within the passed tolerance
    * @param tolerance
    */
  abstract class TolerantEquality[A]
    (val tolerance: A)
    extends Equality[A] {

    protected def add(x: A, amt: A): A
    protected def subtract(x: A, amt: A): A
    //can't use new because of type erasure
    protected def newInstance(x: String): A
    //for some reason A <: Comparable[A] doesn't work...
    //neither does A <: Ordering[A]
    protected def cmp(x: A, y: A): Int

    override def areEqual(a: A, b: Any): Boolean = {
      def assertCompareTo(i: Int): Unit = {
        assert(i == -1 || i == 0 || i == 1)
      }

      def eq(x: A, y: A): Boolean = {
        val upper = add(y, tolerance)
        val lower = subtract(y, tolerance)

        val cmpUpper = cmp(a, upper)
        val cmpLower = cmp(a, lower)
        //sanity check compareTo results
        assertCompareTo(cmpUpper)
        assertCompareTo(cmpLower)

        //x must be <= upper and >= lower
        (cmpUpper  == -1 || cmpUpper == 0) &&
          (cmpLower == 1 || cmpLower == 0)
      }

      //TODO: A is eliminated by type erasure
      (b.isInstanceOf[A] &&
        eq(a, b.asInstanceOf[A])) ||
        //if b is not an instance of the expected type
        //try and convert it
        Try(eq(a, newInstance(b.toString))).getOrElse(false)
    }
  }

  class BigDecimalEquality(override val tolerance: BigDecimal)
    extends TolerantEquality[BigDecimal](tolerance) {
    override protected def add(x: BigDecimal, amt: BigDecimal): BigDecimal =
      x.add(amt)

    override protected def subtract(x: BigDecimal, amt: BigDecimal): BigDecimal =
      x.subtract(amt)

    override protected def newInstance(x: String): BigDecimal = new BigDecimal(x)

    override protected def cmp(x: BigDecimal, y: BigDecimal): Int = x.compareTo(y)
  }

  class DoubleEquality(override val tolerance: Double)
    extends TolerantEquality[Double](tolerance) {
    override protected def add(x: Double, amt: Double): Double = x + amt

    override protected def subtract(x: Double, amt: Double): Double = x - amt

    override protected def newInstance(x: String): Double = x.toDouble

    override protected def cmp(x: Double, y: Double): Int = x.compareTo(y)
  }

  class IntEquality(override val tolerance: Int)
    extends TolerantEquality[Int](tolerance) {
    override protected def add(x: Int, amt: Int): Int = x + amt

    override protected def subtract(x: Int, amt: Int): Int = x - amt

    override protected def newInstance(x: String): Int = x.toInt

    override protected def cmp(x: Int, y: Int): Int = x.compareTo(y)
  }
}

