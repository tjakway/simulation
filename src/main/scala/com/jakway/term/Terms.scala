package com.jakway.term

import scala.{math => M}
import com.jakway.term.NumericTypeFactory.TrigFunctions

trait Term


//TODO: so far this is all just interpreter implementation, not term grammar

trait Function[I, O] extends Term {
  def apply: I => O
}

//notice they're reversed
trait InvertibleFunction[O, I] extends Function[I, O] {
  def inverse: Function[O, I]
}

trait NumericType[A] {
  val sin: InvertibleFunction[A, A]
  val cos: InvertibleFunction[A, A]
  val tan: InvertibleFunction[A, A]

  val arcsin: InvertibleFunction[A, A]
  val arccos: InvertibleFunction[A, A]
  val arctan: InvertibleFunction[A, A]

  val pow: InvertibleFunction[(A, A), A]
}

//TODO: make a class NumericTypeFactory that has all the object
//construction boilerplate and instead takes all the functions
//as constructor arguments

object NumericTypeFactory {
  case class TrigFunctions[A](
      sin: A => A,
      cos: A => A,
      tan: A => A,

      arcsin: A => A,
      arccos: A => A,
      arctan: A => A)
}

/**
  * take functions, produce term objects
  * @param t
  * @tparam A
  */
class NumericTypeFactory[A](t: TrigFunctions[A], pPow: (A, A) => A) extends NumericType[A] {
  //trig functions

  override val sin: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = arcsin

      override def apply = t.sin
    }

  override val cos: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = arccos

      override def apply = t.cos
    }

  override val tan: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = arctan

      override def apply = t.tan
    }

  override val arcsin: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = sin

      override def apply = t.arcsin
    }


  override val arccos: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = cos

      override def apply = t.arccos
    }

  override val arctan: InvertibleFunction[A, A] =
    new InvertibleFunction[A, A] {
      override def inverse = tan

      override def apply = t.arctan
    }

  //other misc. things
  override val pow: InvertibleFunction[(A, A), A] =
    new InvertibleFunction[(A, A), A] {
      override def apply: (A, A) => A = pPow

      override def inverse: Function[A, (A, A)] = ???
    }
}

object DoublePrecision extends NumericTypeFactory[Double](
  TrigFunctions(
    M.sin, M.cos, M.tan,
    M.asin, M.acos, M.atan
  )
)

class EnvParameters {
}
