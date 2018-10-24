package com.jakway.term.simplifier
import com.jakway.term.{HasSubterms, Operation, Term}
import com.jakway.term.numeric.types.{NumericType, SimError}

class InverseIdentitySimplifier extends Simplifier {
  import InverseIdentitySimplifier._


  override def simplify(t: Term): Either[SimError, Term] = t match {
      //input sort to respective sub-objects
    case o@Operation(subterms) if subterms.length == 1 => OneSubterm(o)
    case o@Operation(subterms) if subterms.length > 1 => MultipleSubterms(o)
      //pass all other input through as is
    case x => Right(x)
  }
}


object InverseIdentitySimplifier {
  case class InverseIdentitySimplifierError(override val msg: String)
    extends SimError(msg)

  object OneSubterm {
    def apply(h: Operation): Either[SimError, Term] = {
      if(h.subterms.length != 1) {
        Left(InverseIdentitySimplifierError(s"Expected $h" +
          s" to only have 1 subterm"))
      } else {
        h match {
          case outer@Operation(Seq( inner@Operation(Seq(x))))
            //compare the outer and inner ignoring UUID
            //f^-1(f(x)) = x and f(f^-1(x)) = x
            if outer.inverted.map(_.matches(inner)) == Right(true) ||
              inner.inverted.map(_.matches(outer)) == Right(true) => {
            Right(x)
          }
          //return other input as-is
          case x => Right(x)
        }
      }
    }
  }

  object MultipleSubterms {
    def apply(h: Operation): Either[SimError, Term] = {
      //TODO: implement
      ???
    }
  }
}
