package com.jakway.term.simplifier

import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.{BinaryNumericOperation, HasSubterms, Operation, Term}

trait Simplifier {
  def simplify(t: Term): Either[SimError, Term]
}
