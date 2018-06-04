package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Term

trait Operation extends Term {
  def inverse: Term => Term
  def identity: Term
}

trait NumericTerm[N <: NumericType[M], M] extends Term

trait NumericOperation[N <: NumericType[M], M] extends Operation {
  def litIdentity: Literal[N, M]

  override def identity: Term = litIdentity
}

case class Literal[N <: NumericType[M], M](value: String)
  extends NumericTerm[N, M]

case class Variable[N <: NumericType[M], M](name: String, description: String)
  extends NumericTerm[N, M]

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]

trait BinaryTerm[T <: Term] extends Term {
  val left: T
  val right: T
}

trait ChiralInvertible[T <: Term] extends BinaryTerm[T] with Term {
  def inverseLeft: T
  def inverseRight: T
}

trait BinaryNumericOperation[N <: NumericType[M], M]
  extends NumericOperation[N, M]
  with ChiralInvertible[NumericTerm[N, M]] {
  override def inverseLeft: NumericTerm[N, M] = inverse(left).asInstanceOf[NumericTerm[N, M]]
  override def inverseRight: NumericTerm[N, M] = inverse(right).asInstanceOf[NumericTerm[N, M]]
}


case class Add[N <: NumericType[M], M](
                override val left: NumericTerm[N, M],
                override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverse: Term => Term =
    (term: Term) =>
      Add(Negative(term.asInstanceOf[NumericTerm[N, M]]),
        term.asInstanceOf[NumericTerm[N, M]])

  override def litIdentity: Literal[N, M] = Literal[N, M]("0")
}

case class Multiply[N <: NumericType[M], M](
                                        override val left: NumericTerm[N, M],
                                        override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverse: Term => Term =
    (term: Term) =>
      Multiply(Negative(term.asInstanceOf[NumericTerm[N, M]]),
        term.asInstanceOf[NumericTerm[N, M]])

  override def litIdentity: Literal[N, M] = Literal[N, M]("1")
}

object IdentityFunction extends Term

trait NumericFunction[N <: NumericType[M], M] extends Operation {
  override def identity: Term = IdentityFunction
}

trait TrigFunction[N <: NumericType[M], M] extends NumericFunction[N, M]

case class Sin[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arcsin.apply
}
case class Cos[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arccos.apply
}
case class Tan[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arctan.apply
}
case class Arcsin[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply
}
case class Arccos[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Cos.apply
}
case class Arctan[N <: NumericType[M], M](t: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply
}

//equals is NOT a term--it's an equation
case class Equals(left: Term, right: Term)