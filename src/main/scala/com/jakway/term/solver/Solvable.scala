package com.jakway.term.solver

import com.jakway.term.elements.{Equation, Term}

trait Solvable {
  val sideToSimplify: Term
  val otherSide: Term

  def reverse(): Solvable

  def contains(t: Term): Boolean
}


object Solvable {
  def apply(sideToSimplify: Term, otherSide: Term): Solvable =
    Equation.apply(sideToSimplify, otherSide)
}