package com.jakway.term

import scala.reflect.runtime.universe._
import com.jakway.term.numeric.types.{NumericType, SimError}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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
