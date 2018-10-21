package com.jakway.term

import java.util.UUID

import com.jakway.term.numeric.types.{NumericType, SimError}

import scala.reflect.ClassTag

trait Term {
  def contains(t: Term): Boolean

  val uniqueId: UUID = java.util.UUID.randomUUID()
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
}

case class Literal[N <: NumericType[M], M](value: String)
  extends NumericTerm[N, M]
  with UnnestedTerm

case class Variable[N <: NumericType[M], M](name: String, description: Option[String])
  extends NumericTerm[N, M]
  with UnnestedTerm {

  /**
    * check whether these represent the same variable
    * @param other
    * @return
    */
  def sameVariable(other: Variable[N, M]) = name == other.name
}

object Variable {
  def apply[N <: NumericType[M], M](name: String): Variable[N, M] = new Variable[N, M](name, None)
  def apply[N <: NumericType[M], M](name: String, description: String): Variable[N, M]
    = new Variable[N, M](name, Some(description))
}

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends NumericTerm[N, M]
  with HasSubterms {
  override val subterms: Seq[Term] = Seq(arg)

  override def newInstance: NewInstanceF =
    subterms => {
      Negative(assertCast[NumericTerm[N,M]](assertArity(1, subterms)(0)))
    }
}

trait HasSubterms extends Term {
  val subterms: Seq[Term]

  def contains(t: Term): Boolean = equals(t) || subterms.contains(t)

  type NewInstanceF = Seq[Term] => Term
  def newInstance: NewInstanceF

  case class NewInstanceException(override val msg: String)
    extends SimError(msg)

  protected def assertArity(arity: Int, subterms: Seq[Term]): Seq[Term] =
    if(subterms.length != arity) {
      throw new NewInstanceException(s"Expected arity of $arity" +
        s" but got new subterms: $subterms")
    } else subterms

  protected def assertCast[A <: Term](t: Term)(implicit tag: ClassTag[A]): A = {
    try {
      t.asInstanceOf[A]
    } catch {
      case e: Throwable => {
        val newException = NewInstanceException(s"Error while casting $t " +
          s"to " + tag.runtimeClass.getName)
        newException.addSuppressed(e)
        throw newException
      }
    }
  }
}

object HasSubterms {
  def unapply(h: HasSubterms): Option[Seq[Term]] =
    Some(h.subterms)
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

  protected def mkNewInstance[X <: BinaryNumericOperation[N, M]]
    (constructor: (NumericTerm[N, M], NumericTerm[N, M]) => X): NewInstanceF = {

    (withSubterms: Seq[Term]) => {
      val Seq (l, r) = assertArity (2, withSubterms).take (2)
      constructor(assertCast(l), assertCast(r))
    }
  }
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

  override def newInstance: NewInstanceF = mkNewInstance(Add.apply)
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

  override def newInstance: NewInstanceF = mkNewInstance(Multiply.apply)
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


  protected def mkNewInstance[X <: OneArgumentFunction[N, M]]
    (constructor: NumericTerm[N, M] => X): NewInstanceF = {

    (withSubterms: Seq[Term]) => {
      val Seq (a) = assertArity (1, withSubterms).take (1)
      constructor(assertCast(a))
    }
  }
}

trait TrigFunction[N <: NumericType[M], M]
  extends OneArgumentFunction[N, M]

case class Sin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arcsin.apply

  override def newInstance: NewInstanceF = mkNewInstance[Sin[N,M]](Sin.apply)
}
case class Cos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arccos.apply

  override def newInstance: NewInstanceF = mkNewInstance[Cos[N,M]](Cos.apply)
}
case class Tan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Arctan.apply

  override def newInstance: NewInstanceF = mkNewInstance[Tan[N,M]](Tan.apply)
}
case class Arcsin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply

  override def newInstance: NewInstanceF = mkNewInstance[Arcsin[N,M]](Arcsin.apply)
}
case class Arccos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Cos.apply

  override def newInstance: NewInstanceF = mkNewInstance[Arccos[N,M]](Arccos.apply)
}
case class Arctan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverse: Term => Term = Sin.apply

  override def newInstance: NewInstanceF = mkNewInstance[Arctan[N,M]](Arctan.apply)
}

/**
  * Equation is intentionally not an instance of Term
  * @param left
  * @param right
  */
case class Equation(left: Term, right: Term) {

  def contains(t: Term): Boolean =
    left.contains(t) || right.contains(t)
}

class Examples[N <: NumericType[M], M] {
  val x: Equation = Equation(Variable("x", "foo"),
    Add(Sin(Literal("200")), Cos(Literal("250"))))
}