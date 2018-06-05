package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Term {
  def contains(t: Term): Boolean
}

trait Operation extends Term with HasSubterms {
  def inverse: Term => Term
  def identity: Term
}

trait NumericTerm[N <: NumericType[M], M] extends Term

trait NumericOperation[N <: NumericType[M], M] extends Operation {
  def litIdentity: Literal[N, M]

  override def identity: Term = litIdentity
}

/**
  * if a term doesn't contain any subterms then just
  * check if this is the term we're looking for
  */
trait UnnestedTerm extends Term {
  override def contains(t: Term): Boolean = equals(t)
}

case class Literal[N <: NumericType[M], M](value: String)
  extends NumericTerm[N, M]
  with UnnestedTerm

case class Variable[N <: NumericType[M], M](name: String, description: String)
  extends NumericTerm[N, M]
  with UnnestedTerm

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]
  with UnnestedTerm

trait HasSubterms extends Term {
  val subterms: Seq[Term]

  override def contains(t: Term): Boolean = equals(t) || subterms.contains(t)
}

trait BinaryTerm[T <: Term] extends Term with HasSubterms {
  val left: T
  val right: T

  override val subterms: Seq[Term] = Seq(left, right)
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

object IdentityFunction extends UnnestedTerm

trait NumericFunction[N <: NumericType[M], M] extends Operation {
  override def identity: Term = IdentityFunction
}


//TODO: ...is this the right way to do this?
trait OneArgumentFunction extends Operation {
  //trig functions only take 1 argument
  val argument: Term

  override val subterms: Seq[Term] = Seq(argument)
}

trait TrigFunction[N <: NumericType[M], M]
  extends NumericFunction[N, M]
  with OneArgumentFunction

case class Sin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arcsin.apply
}
case class Cos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arccos.apply
}
case class Tan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arctan.apply
}
case class Arcsin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply
}
case class Arccos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Cos.apply
}
case class Arctan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply
}

//equals is NOT a term--it's an equation
case class Equals(left: Term, right: Term)