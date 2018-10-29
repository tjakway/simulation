package com.jakway.term

import java.util.UUID

import com.jakway.term.HasSubterms.NewInstanceF
import com.jakway.term.NewInstanceHelpers.{Arity1MkNewInstance, Arity2MkNewInstance}
import com.jakway.term.numeric.types.{NumericType, SimError}

import scala.reflect.ClassTag

trait Term {
  def contains(t: Term): Boolean

  val uniqueId: UUID = java.util.UUID.randomUUID()

  /**
    * equals ignoring UUID
    * @param other
    * @return
    */
  def matches(other: Term): Boolean

  def sameType(other: Any): Boolean =
    getClass() == other.getClass()
}


trait Operation extends Term with HasSubterms {
  def inverseConstructorE: Seq[Term] => Either[SimError, Term]

  def identity: Term
  val numArguments: Int

  class InverseError(override val msg: String)
    extends SimError(msg)

  /**
    * Calls the constructor of this type's inverse
    * Read as "the inverse of Add is Subtract",
    * not "the inverse of 2 + 3 is 2 - 3"
    * For the latter, called [[inverted]]
    * @return
    */
  final def inverse: Seq[Term] => Either[SimError, Term] =
    (args: Seq[Term]) => {
      if(args.length != numArguments) {
        Left(new InverseError("Incorrect number of arguments " +
          s"(expected $numArguments, got ${args.length}: $args"))
      } else {
        inverseConstructorE(args)
      }
    }

  /**
    * inverse constructor applied to this Operation's subterms
    * @return
    */
  final def inverted: Either[SimError, Term] =
    inverse(subterms)

  def inverseClass: Class[_ <: Term] = inverted.map(_.getClass()).right.get

  def isInverseTypeOf(t: Term): Boolean =
    t.getClass == inverseClass
}

object Operation {
  case class InverseArgumentsError(override val msg: String)
    extends SimError(msg)

  case class InverseConstructorCastError(override val msg: String)
    extends SimError(msg)

  /**
    * inverse constructor applied to this Operation's subterms
    * @return
    */
  final def inverted: Either[SimError, Term] =
    inverse(subterms)
}

object Operation {
  case class InverseArgumentsError(override val msg: String)
    extends SimError(msg)

  case class InverseConstructorCastError(override val msg: String)
    extends SimError(msg)

  /**
    * Like assertCast but return an algebraic error type
    * @param t
    * @param tag
    * @tparam A
    * @return
    */
  def checkCast[A <: Term](t: Term)(implicit tag: ClassTag[A]): Either[SimError, A] = {
    try {
      Right(t.asInstanceOf[A])
    } catch {
      case e: Throwable => {
        val newException = InverseConstructorCastError(s"Error while casting $t " +
          s"to " + tag.runtimeClass.getName + s" caused by $e")
        newException.addSuppressed(e)
        Left(newException)
      }
    }
  }

  /**
    * Extract subterms from an Operation
    * @param o
    * @return
    */
  def unapply(o: Operation): Option[Seq[Term]] = HasSubterms.unapply(o)
}

trait CommutativeOperation extends Operation {
  /**
    * An iterator over every permutation of this operation's
    * arguments
    * @return
    */
  def permutations: Iterator[CommutativeOperation] =
    subterms.permutations
      .map(newInstance(_).asInstanceOf[CommutativeOperation])
}

object CommutativeOperation {
  def unapply(o: CommutativeOperation): Option[Seq[Term]] = Operation.unapply(o)
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
  with UnnestedTerm {

  override def matches(other: Term) = {
    sameType(other) &&
      value == other.asInstanceOf[Literal[N, M]].value
  }
}

case class Variable[N <: NumericType[M], M](name: String, description: Option[String])
  extends NumericTerm[N, M]
  with UnnestedTerm {

  /**
    * check whether these represent the same variable
    * @param other
    * @return
    */
  def sameVariable(other: Variable[N, M]) = name == other.name

  override def matches(other: Term) = {
    sameType(other) &&
      sameVariable(other.asInstanceOf[Variable[N, M]])
  }
}

object Variable {
  def apply[N <: NumericType[M], M](name: String): Variable[N, M] = new Variable[N, M](name, None)
  def apply[N <: NumericType[M], M](name: String, description: String): Variable[N, M]
    = new Variable[N, M](name, Some(description))
}

object Variable {
  def apply[N <: NumericType[M], M](name: String): Variable[N, M] = new Variable[N, M](name, None)
  def apply[N <: NumericType[M], M](name: String, description: String): Variable[N, M]
    = new Variable[N, M](name, Some(description))
}

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends OneArgumentFunction[N, M]
  with NumericOperation[N,M] with HasSubterms {
  override val subterms: Seq[Term] = Seq(arg)

  override def newInstance: NewInstanceF =
    subterms => {
      Negative(HasSubterms.assertCast[NumericTerm[N,M]](HasSubterms.assertArity(1, subterms)(0)))
    }

  override def litIdentity: Literal[N, M] = Literal("0")

  override val numArguments: Int = 1
  override val argument: Term = arg

  /**
    * Negative is its own inverse
    * @return
    */
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Negative.apply)
}

object HasSubterms {
  type NewInstanceF = Seq[Term] => Term


  case class NewInstanceException(override val msg: String)
    extends SimError(msg)

  def assertArity(arity: Int, subterms: Seq[Term]): Seq[Term] =
    if(subterms.length != arity) {
      throw new NewInstanceException(s"Expected arity of $arity" +
        s" but got new subterms: $subterms")
    } else subterms

  def assertCast[A <: Term](t: Term)(implicit tag: ClassTag[A]): A = {
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



  def unapply(h: HasSubterms): Option[Seq[Term]] =
    Some(h.subterms)
}

trait HasSubterms {
  val subterms: Seq[Term]

  def contains(t: Term): Boolean = equals(t) || subterms.contains(t)
  def newInstance: NewInstanceF


  override def matches(other: Term): Boolean = {
    sameType(other) &&
      subterms == other.asInstanceOf[HasSubterms].subterms
  }
}

trait HasSubterms extends Term {
  val subterms: Seq[Term]

  def contains(t: Term): Boolean = equals(t) || subterms.contains(t)
  def newInstance: NewInstanceF


  override def matches(other: Term): Boolean = {
    sameType(other) &&
      subterms == other.asInstanceOf[HasSubterms].subterms
  }
}

trait BinaryTerm[T <: Term] extends Term with HasSubterms {
  val left: T
  val right: T

  override val subterms: Seq[Term] = Seq(left, right)
}

object NewInstanceHelpers {
  def arity2MkNewInstance[N <: NumericType[M], M, X <: Term]
  (constructor: (NumericTerm[N, M], NumericTerm[N, M]) => X): NewInstanceF = {
    (withSubterms: Seq[Term]) => {
      val Seq (l, r) = HasSubterms.assertArity (2, withSubterms).take (2)
      constructor(HasSubterms.assertCast(l), HasSubterms.assertCast(r))
    }
  }

  def arity1MkNewInstance[N <: NumericType[M], M, X <: Term]
  (constructor: NumericTerm[N, M] => X): NewInstanceF = {

    (withSubterms: Seq[Term]) => {
      val Seq (a) = HasSubterms.assertArity(1, withSubterms).take (1)
      constructor(HasSubterms.assertCast(a))
    }
  }

  trait HasConstructorArgType[N <: NumericType[M], M] {
    type ConstructorArgType = NumericTerm[N, M]
  }

  trait Arity1MkNewInstance[N <: NumericType[M], M]
    extends HasConstructorArgType[N, M] {
    def mkNewInstance[X <: Term]: (ConstructorArgType => X) => NewInstanceF
    = NewInstanceHelpers.arity1MkNewInstance
  }

  trait Arity2MkNewInstance[N <: NumericType[M], M]
    extends HasConstructorArgType[N, M] {
    def mkNewInstance[X <: Term]: ((ConstructorArgType, ConstructorArgType) => X) => NewInstanceF
      = NewInstanceHelpers.arity2MkNewInstance
  }
}


trait BinaryNumericOperation[N <: NumericType[M], M]
  extends NumericOperation[N, M]
  with BinaryTerm[NumericTerm[N, M]]
  with Arity2MkNewInstance[N, M] {

  override val numArguments: Int = 2

  /**
    * mkInverseConstructorE for 2-arity types
    * @return
    */
  def mkInverseConstructorE:
    ((ConstructorArgType, ConstructorArgType) => Term) => Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity2MkInverseConstructorE
}


case class Add[N <: NumericType[M], M](
                override val left: NumericTerm[N, M],
                override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Subtract.apply)


  override def litIdentity: Literal[N, M] = Literal[N, M]("0")

  override def newInstance: NewInstanceF = mkNewInstance(Add.apply)
}

case class Subtract[N <: NumericType[M], M](
                                  override val left: NumericTerm[N, M],
                                  override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Add.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("0")

  override def newInstance: NewInstanceF = mkNewInstance(Subtract.apply)
}

case class Multiply[N <: NumericType[M], M](
                                        override val left: NumericTerm[N, M],
                                        override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Divide.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("1")

  override def newInstance: NewInstanceF = mkNewInstance(Multiply.apply)
}

case class Divide[N <: NumericType[M], M](
   val dividend: NumericTerm[N, M],
   val numerator: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override val left = dividend
  override val right = numerator

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Multiply.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("1")

  override def newInstance: NewInstanceF = mkNewInstance(Divide.apply)
}

object IdentityFunction extends UnnestedTerm {
  override def matches(other: Term) =
    sameType(other)
}

trait NumericFunction[N <: NumericType[M], M]
  extends Operation
  with NumericTerm[N, M] {

  override def identity: Term = IdentityFunction

  val arguments: Seq[Term]

  //a functions arguments are subterms
  override val subterms: Seq[Term] = arguments
}

object InverseConstructorHelpers {

  /**
    * mkInverseConstructorE for 1-arity types
    *
    * takes a 1-arity constructor and returns a function that
    * will construct that type from a Seq[Term]
    *
    * see https://stackoverflow.com/questions/50790011/scala-lambda-cannot-be-cast-to-classtag-error
    * for getting a ClassTag when that generic also needs type constraints
    * @return
    */
  def arity1MkInverseConstructorE[ConstructorArgType <: Term : ClassTag]:
  (ConstructorArgType => Term) => Seq[Term] => Either[SimError, Term] = {
    ctor =>
      (args: Seq[Term]) =>
        val numArguments = 1 //arity 1 constructor
        val Seq(le) = args.take(numArguments)
        val res: Either[SimError, ConstructorArgType] =
          Operation.checkCast[ConstructorArgType](le)

        res.map(ctor)
  }


  /**
    * mkInverseConstructorE for 2-arity types
    * @return
    */
  def arity2MkInverseConstructorE[ConstructorArgType <: Term : ClassTag]:
  ((ConstructorArgType, ConstructorArgType) => Term) => Seq[Term] => Either[SimError, Term] = {
    ctor =>
      (args: Seq[Term]) =>
        val numArguments = 2
        val Seq(le, re) = args.take(numArguments)
        val res: Either[SimError, (ConstructorArgType, ConstructorArgType)] = for {
          l <- Operation.checkCast[ConstructorArgType](le)
          r <- Operation.checkCast[ConstructorArgType](re)
        } yield (l, r)

        res.map(x => ctor(x._1, x._2))
  }
}


trait OneArgumentFunction[N <: NumericType[M], M]
  extends NumericFunction[N, M]
  with Arity1MkNewInstance[N, M] {
  //trig functions only take 1 argument
  val argument: Term
  override val arguments: Seq[Term] = Seq(argument)

  override val subterms: Seq[Term] = Seq(argument)

  override val numArguments: Int = 1

  def mkInverseConstructorE:
  (ConstructorArgType => Term) => Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity1MkInverseConstructorE
}

abstract class TwoArgumentFunction[N <: NumericType[M], M]
  (val arg1: Term, val arg2: Term)
  extends NumericFunction[N, M]
  with Arity2MkNewInstance[N, M] {

  override val arguments: Seq[Term] = Seq(arg1, arg2)
  override val subterms: Seq[Term] = arguments

  override val numArguments: Int = 2

  def mkInverseConstructorE: ((ConstructorArgType, ConstructorArgType) => Term) =>
    Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity2MkInverseConstructorE
}

abstract class TwoArgumentFunction[N <: NumericType[M], M]
  (val arg1: Term, val arg2: Term)
  extends NumericFunction[N, M] {

  override val arguments: Seq[Term] = Seq(arg1, arg2)
  override val subterms: Seq[Term] = arguments

  override val numArguments: Int = 2

  def mkInverseConstructorE: ((ConstructorArgType, ConstructorArgType) => Term) =>
    Seq[Term] => Either[SimError, Term] =
    InverseConstructorHelpers.arity2MkInverseConstructorE
}

trait TrigFunction[N <: NumericType[M], M]
  extends OneArgumentFunction[N, M]

case class Sin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arcsin.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Sin[N,M]](Sin.apply)
}
case class Cos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arccos.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Cos[N,M]](Cos.apply)
}
case class Tan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arctan.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Tan[N,M]](Tan.apply)
}
case class Arcsin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Sin.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arcsin[N,M]](Arcsin.apply)
}
case class Arccos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Cos.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arccos[N,M]](Arccos.apply)
}
case class Arctan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Tan.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arctan[N,M]](Arctan.apply)
}

case class Logarithm[N <: NumericType[M], M]
  (val base: NumericTerm[N, M],
   val of: NumericTerm[N, M])
  extends TwoArgumentFunction[N, M](base, of) {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Power.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Logarithm[N,M]](Logarithm.apply)
}

case class Power[N <: NumericType[M], M](
      val base: NumericTerm[N, M], val exponent: NumericTerm[N, M])
  extends TwoArgumentFunction[N, M](base, exponent) {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Logarithm.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Power[N,M]](Power.apply)
}

class NaturalLogarithm[N <: NumericType[M], M] {
  //TODO: need to handle this specially
  val e: NumericTerm[N, M] = Literal("e")

  /**
    * returns a log in base e
    * @param of
    * @return
    */
  def naturalLog(of: NumericTerm[N, M]): Logarithm[N, M] =
    Logarithm(e, of)
}



/**
  * Equation is intentionally not an instance of Term
  * @param left
  * @param right
  */
case class Equation(left: Term, right: Term) {

  def contains(t: Term): Boolean =
    left.contains(t) || right.contains(t)

  /**
    * compares terms using .matches
    * @param o
    * @return
    */
  override def equals(o: Any): Boolean = {
    def eq(e: Equation): Boolean = {
      left.matches(e.left) && right.matches(e.right)
    }

    o.isInstanceOf[Any] && eq(o.asInstanceOf[Equation])
  }
}

class Examples[N <: NumericType[M], M] {
  val x: Equation = Equation(Variable("x", "foo"),
    Add(Sin(Literal("200")), Cos(Literal("250"))))
}
