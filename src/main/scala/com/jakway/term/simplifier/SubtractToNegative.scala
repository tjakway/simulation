package com.jakway.term.simplifier
import com.jakway.term.elements.{Add, Negative, Subtract, Term}
import com.jakway.term.numeric.types.{NumericType, SimError}

class SubtractToNegative[N <: NumericType[M], M] extends Simplifier {
  override def simplify(t: Term): Either[SimError, Term] =
    t match {
      case Subtract(left, right) => {
        Right(Add(left, Negative(right)))
      }
      case _ => Right(t)
  }
}
