package com.jakway.term.simplifier

import com.jakway.term.elements.{BinaryNumericOperation, Term}
import com.jakway.term.numeric.types.{NumericType, SimError}

trait Simplifier {
  def simplify(t: Term): Either[SimError, Term]
}
