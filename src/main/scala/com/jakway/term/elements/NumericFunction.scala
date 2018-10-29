package com.jakway.term.elements

import com.jakway.term.numeric.types.NumericType

trait NumericFunction[N <: NumericType[M], M]
  extends Operation
  with NumericTerm[N, M] {

  override def identity: Term = IdentityFunction

  val arguments: Seq[Term]

  //a functions arguments are subterms
  override val subterms: Seq[Term] = arguments
}
