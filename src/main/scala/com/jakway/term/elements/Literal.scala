package com.jakway.term.elements

import com.jakway.term.numeric.types.NumericType

case class Literal[N <: NumericType[M], M](value: String)
  extends NumericTerm[N, M]
  with UnnestedTerm {

  override def matches(other: Term) = {
    sameType(other) &&
      value == other.asInstanceOf[Literal[N, M]].value
  }
}
