package com.jakway.term.numeric.types

object NumericTypeFactory {
  //TODO: think about how to handle inverses
  case class ArithmeticOperations[A](
                                   // plus: A => A => A,
                                   // times: A => A => A
                                    )

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
 *
  * @param t
  * @tparam A
  */
abstract class NumericTypeFactory[A](t: NumericTypeFactory.TrigFunctions[A],
                            pPow: A => A => A,
                            pRoot: A => A => A) extends NumericType[A] {
  //trig functions

  override val sin: TrigFunction =
    new TrigFunction {
      override def inverse = arcsin

      override def compute = t.sin
    }

  override val cos: TrigFunction =
    new TrigFunction {
      override def inverse = arccos

      override def compute = t.cos
    }

  override val tan: TrigFunction =
    new TrigFunction {
      override def inverse = arctan

      override def compute = t.tan
    }

  override val arcsin: TrigFunction =
    new TrigFunction {
      override def inverse = sin

      override def compute = t.arcsin
    }


  override val arccos: TrigFunction =
    new TrigFunction {
      override def inverse = cos

      override def compute = t.arccos
    }

  override val arctan: TrigFunction =
    new TrigFunction {
      override def inverse = tan

      override def compute = t.arctan
    }

  //other misc. things
  override val pow: BinaryMathFunction =
    new BinaryMathFunction {
      override def compute = pPow

      override def inverse = root
    }


  override val root: BinaryMathFunction =
    new BinaryMathFunction {
      override def compute: A => A => A = pRoot

      override def inverse = pow
    }
}