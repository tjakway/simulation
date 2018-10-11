package com.jakway.term

import com.jakway.term.numeric.types.NumericType

trait Term {
  def contains(t: Term): Boolean

  def foldLeftSpecified[B](foldOverBranches: Boolean)
                          (a: B)
                          (f: (B, Term) => B): B
}

object Term {
}

trait Operation extends Term with HasSubterms {
  def inverse: Term => Term
  def identity: Term
}

trait NumericTerm[N <: NumericType[M], M] extends Term

trait NumericOperation[N <: NumericType[M], M]
  extends Operation
  with NumericTerm[N, M] {

  def litIdentity: Literal[N, M]

  override def identity: Term = litIdentity
}

/**
  * if a term doesn't contain any subterms then just
  * check if this is the term we're looking for
  */
trait UnnestedTerm extends Term {
  override def contains(t: Term): Boolean = equals(t)

  /**
    * don't recurse any further
    *
    * @param a
    * @param f
    * @tparam B
    * @return
    */
  /*override def foldLeft[B](a: B)(f: (B, Term) => B): B =
    f(a, this)*/

}

case class Literal[N <: NumericType[M], M](value: String)
  extends NumericTerm[N, M]
  with UnnestedTerm

case class Variable[N <: NumericType[M], M](name: String, description: Option[String])
  extends NumericTerm[N, M]
  with UnnestedTerm

object Variable {
  def apply[N <: NumericType[M], M](name: String): Variable[N, M] = new Variable[N, M](name, None)
  def apply[N <: NumericType[M], M](name: String, description: String): Variable[N, M]
    = new Variable[N, M](name, Some(description))
}

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]
  with UnnestedTerm

/**
  * this is intentionally not an instance of Term so that Equation can implement it
  */
trait HasSubterms {
  val subterms: Seq[Term]

  def contains(t: Term): Boolean = equals(t) || subterms.contains(t)

  def newInstance(withSubterms: Seq[Term]): Term

  def foldLeft[B](a: B)(f: (B, Term) => B): B =
    subterms.foldLeft[B](a)(f)

  def foldLeftSpecified[B](foldOverBranches: Boolean)
                           (a: B)
                           (f: (B, Term) => B): B =
    subterms.foldLeft[B](a) {
      case (acc, thisTerm) => thisTerm.foldLeftSpecified(foldOverBranches)(a)(f)
    }
}

trait BinaryTerm[T <: Term] extends Term with HasSubterms {
  val left: T
  val right: T

  override val subterms: Seq[Term] = Seq(left, right)

  override def foldLeftSpecified[B](foldOverBranches:  Boolean)
                                   (a:  B)
                                   (f:  (B, _root_.com.jakway.term.Term) => B): B = super.foldLeftSpecified(foldOverBranches)(a)(f)
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

trait NumericFunction[N <: NumericType[M], M]
  extends Operation
  with NumericTerm[N, M] {

  override def identity: Term = IdentityFunction

  val arguments: Seq[Term]

  //a functions arguments are subterms
  override val subterms: Seq[Term] = arguments
}


trait OneArgumentFunction[N <: NumericType[M], M] extends NumericFunction[N, M] {
  //trig functions only take 1 argument
  val argument: Term
  override val arguments: Seq[Term] = Seq(argument)

  override val subterms: Seq[Term] = Seq(argument)
}

trait TrigFunction[N <: NumericType[M], M]
  extends OneArgumentFunction[N, M]

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

/**
  * Equation is intentionally not an instance of Term
  * @param left
  * @param right
  */
case class Equation(left: Term, right: Term)
  extends HasSubterms {
  override val subterms: Seq[Term] = Seq(left, right)
}

class Examples[N <: NumericType[M], M] {
  val x: Equation = Equation(Variable("x", "foo"),
    Add(Sin(Literal("200")), Cos(Literal("250"))))
}