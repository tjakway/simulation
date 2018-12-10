package com.jakway.term.elements

import com.jakway.term.numeric.errors.SimError

import scala.reflect.ClassTag

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