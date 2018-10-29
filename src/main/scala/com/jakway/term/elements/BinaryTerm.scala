package com.jakway.term.elements

trait BinaryTerm[T <: Term] extends Term with HasSubterms {
  val left: T
  val right: T

  override val subterms: Seq[Term] = Seq(left, right)
}
