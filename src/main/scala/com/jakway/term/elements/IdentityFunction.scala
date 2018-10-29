package com.jakway.term.elements

object IdentityFunction extends UnnestedTerm {
  override def matches(other: Term) =
    sameType(other)
}
