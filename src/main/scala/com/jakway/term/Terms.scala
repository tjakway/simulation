package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Term

trait NumericTerm[N <: NumericType[M], M]

case class Literal[N <: NumericType[M], M](value: M)
  extends NumericTerm[N, M]

case class Variable[N <: NumericType[M], M](name: String, description: String)
  extends NumericTerm[N, M]

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]


trait InvertibleTerm extends Term {
  def inverse: Term
}

trait BinaryTerm extends Term {
  val left: Term
  val right: Term
}

trait ChiralInvertible extends BinaryTerm with InvertibleTerm {
  def inverseLeft: Term
  def inverseRight: Term
}


case class Add[N <: NumericType[M], M](
                override val left: InvertibleTerm,
                override val right: InvertibleTerm)
  extends NumericTerm[N, M] with ChiralInvertible {

  override def inverseLeft: Term = left.inverse
  override def inverseRight: Term = right.inverse

  override def inverse: Term = Negative(Add(left, right))
}


case class Multiply(override val left: InvertibleTerm,
                override val right: InvertibleTerm)
  extends ChiralInvertible {

  override def inverseLeft: Term = left.inverse
  override def inverseRight: Term = right.inverse

  override def inverse: Term = Negative(Multiply(left, right))
}

//equals is NOT a term--it's an equation
case class Equals(left: Term, right: Term)