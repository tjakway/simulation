package com.jakway.term.solver

import com.jakway.term.elements.Term

trait Solvable {
  val sideToSimplify: Term
  val otherSide: Term

  def reverse(): Solvable

  def contains(t: Term): Boolean
}
