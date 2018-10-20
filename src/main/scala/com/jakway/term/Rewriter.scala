package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

class Rewriter {
  case class TermNotFoundError(refactorFor: Term, equation: Equation)
    extends SimError(s"Could not find term ${refactorFor} in equation $equation")

  def refactor(refactorFor: Term)(equation: Equation): Either[SimError, Equals] = {
    if(!equation.contains(refactorFor)) {
      Left(TermNotFoundError(refactorFor, equation))

      //check if the equation is already in the desired form
    } else if(equation.left == refactorFor) {
      Right(equation)
    } else {
      //TODO XXX
      TermOperations.mapAll(equation.left) {
        ???
      }
      ???
    }
  }
}

object TermOperations {
  case class CastHasSubtermsError(castTerm: Term, t: Throwable)
    extends SimError(s"Error casting $castTerm to an instance of HasSubterms",
      t)

  /**
    * *** XXX WARNING: *** it is the caller's responsibility to ensure that
    * mapped types conform to the expectation of enclosing terms
    * @param t
    * @param f
    * @return
    */
  def mapAll(t: Term)(f: Term => Term): Term = t match {
    case x: HasSubterms => f(x.newInstance(x.subterms.map(f)))
    case x => f(x)
  }

  def foreach(t: Term)(f: Term => Unit): Unit =
    mapAll(t) { a =>
      f(a)
      a
    }

  def replaceSubterms(replace: HasSubterms => Seq[Term] => Seq[Term])
                     (h: HasSubterms): Either[SimError, HasSubterms] = {
    Try {
      h.newInstance(replace(h)(h.subterms)).asInstanceOf[HasSubterms]
    } match {
      case Success(x) => Right(x)
      case Failure(e) if e.isInstanceOf[ClassCastException] => {
        Left(CastHasSubtermsError(h, e))
      }
      case Failure(x) => Left(new SimError(x))
    }
  }

  def foldSubterms[B](t: Term)(z: B)(f: (B, Term) => B): B = t match {
      //fold over the top level term then subterms
    case h: HasSubterms => h.subterms.foldLeft(f(z, h))(f)
    case _ => f(z, t)
  }

  def findVariables[N <: NumericType[M], M](t: Term): Seq[Variable[N, M]] = {
    val empty: Seq[Variable[N, M]] = Seq()

    //accumulate variables
    foldSubterms(t)(empty) {
      case (variables, x@Variable(_, _)) => {
        variables :+ x
      }
      case (variables, _) => variables
    }
  }

}

class Simplifier {
  def simplifiers[A <: Term]: Set[A => Either[SimError, Term]] = ??? //TODO

  /*
  def mkSimplifier[A <: Term](f: A => Term): A => Either[SimError, Term] = {
    (arg: )
  }*/

  def simplify[N <: NumericType[M], M](t: Term): Either[SimError, Term] = {
    //if there are no subterms, it's already in simplest form
    if(!t.isInstanceOf[HasSubterms]) {
      Right(t)
    } else {
      t match {
        case o: Operation => {
          //remove outer term if it's an operation on identities
          if (o.subterms.forall(_ == o.identity)) {
            //TODO
            // Right(o)
            ???
          } else {
            ???
          }
        }
        case b: BinaryNumericOperation[N, M] => {
          ??? //TODO
        }

        case _ => ???
      }
    }
  }

}
