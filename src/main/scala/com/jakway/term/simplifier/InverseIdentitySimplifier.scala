package com.jakway.term.simplifier
import com.jakway.term.{HasSubterms, Operation, Term, TermOperations}
import com.jakway.term.numeric.types.{NumericType, SimError}
import org.slf4j.{Logger, LoggerFactory}

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
  class InverseIdentitySimplifierError(override val msg: String)
    extends SimError(msg)

  case class IdentityFunctionError(override val msg: String)
    extends InverseIdentitySimplifierError(msg)

  /**
    * make sure that f^-1(f(x)) = id iff f(f^-1(x)) = id
    * @param a
    * @param b
    */
  def assertIdentityLaw(a: Operation, b: Operation): Unit = {
    def err(msg: String): Unit = throw IdentityFunctionError(msg ++
      s"\n\tfor a=$a, b=$b")

    if(a.isInverseTypeOf(b)) {
      if(!b.isInverseTypeOf(a)) {
        err("a.isInverseTypeOf(b) but " +
          "!b.isInverseTypeOf(a)")
      }
    }

    if(b.isInverseTypeOf(a)) {
      if(!a.isInverseTypeOf(b)) {
        err("b.isInverseTypeOf(a) but " +
          "!a.isInverseTypeOf(b)")
      }
    }
  }

  object OneSubterm {
    def apply(h: Operation): Either[SimError, Term] = {
      if(h.subterms.length != 1) {
        Left(new InverseIdentitySimplifierError(s"Expected $h" +
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
    val logger: Logger = LoggerFactory.getLogger(getClass)

    def apply(h: Operation): Either[SimError, Term] = {

      def simplify(x: Operation): Option[Term] = x match {
          //see https://stackoverflow.com/questions/6807540/scala-pattern-matching-on-sequences-other-than-lists/19147469#19147469
          //for pattern matching on Seqs
        case outer@Operation(Seq(innerT@Operation(subArgs) +: _)) => {
          //for some reason pattern matching this doesn't work
          val inner = innerT.asInstanceOf[Operation]
          assertIdentityLaw(outer, inner)
          if(outer.isInverseTypeOf(inner)) {

            val overlappingSubterms =
              TermOperations.getOverlappingSubterms(outer, inner)

            if(outer.subterms.forall(overlappingSubterms.contains)) {
              val disjointTerms = TermOperations.getDisjointSubterms(outer, inner)

              if(disjointTerms.length == 1) {
                Some(disjointTerms.head)
              } else if(disjointTerms.length > 1) {
                logger.debug(s"Length of disjoint terms of " +
                  s"$outer and $inner is >1")
                None
              } else { None }
            } else {
              None
            }
          }
          else {
            None
          }
        }

        case _ => None
      }

      simplify(h).map(Right(_)).getOrElse(Right(h))
    }
  }
}
