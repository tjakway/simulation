package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

import scala.annotation.tailrec

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

      ???
    }
  }
}

object TermOperations {

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
