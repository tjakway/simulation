package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

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
