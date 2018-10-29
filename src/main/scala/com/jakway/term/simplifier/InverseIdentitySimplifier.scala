package com.jakway.term.simplifier

import com.jakway.term._
import com.jakway.term.numeric.types.SimError
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
  def assertIdentityLaw(a: Operation, b: Operation): Boolean = {
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

    a.isInverseTypeOf(b)
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
            if assertIdentityLaw(outer, inner) => Right(x)
          //return other input as-is
          case x => Right(x)
        }
      }
    }
  }

  object MultipleSubterms {
    val logger: Logger = LoggerFactory.getLogger(getClass)

    def apply(h: Operation): Either[SimError, Term] = {

      /**
        * @param x
        * @param dontRecurse flag to prevent infinite recursion when evaluating commutative operations
        * @return
        */
      def simplify(x: Operation, dontRecurse: Boolean = false): Option[Term] = x match {
          //handle commutative operations in the inner term
        case outer@Operation(Seq(innerT +: _))
          if dontRecurse == false => {

          //if the inner term is commutative, recurse over every
          //permutation of its arguments
          if(innerT.isInstanceOf[CommutativeOperation]) {
            innerT.asInstanceOf[CommutativeOperation]
              .permutations
              //return the first permutation that simplifies
              .foldLeft(None: Option[Term]) {
                case (None, t) => simplify(t, true)
                case (Some(x), _) => Some(x)
              }
          } else {
            simplify(x, true)
          }
        }

          //see https://stackoverflow.com/questions/6807540/scala-pattern-matching-on-sequences-other-than-lists/19147469#19147469
          //for pattern matching on Seqs
        case outer@Operation(outerArgs) => {
          def simplifyInnerOperation(inner: Operation): Option[Term] = {
            assertIdentityLaw(outer, inner)
            if (outer.isInverseTypeOf(inner)) {

              val overlappingSubterms =
                TermOperations.getOverlappingSubterms(outer, inner)

              if (overlappingSubterms.forall(outer.subterms.contains)) {
                //exclude the inner operation from the set of disjoint subterms
                val disjointTerms =
                  TermOperations.getDisjointSubterms(outer, inner)
                                .filterNot(_ == inner)

                if (disjointTerms.length == 1) {
                  Some(disjointTerms.head)
                } else if (disjointTerms.length > 1) {
                  logger.debug(s"Length of disjoint terms of " +
                    s"$outer and $inner is >1")
                  None
                } else {
                  None
                }
              } else {
                None
              }
            }
            else {
              None
            }
          }

          val empty: Option[Term] = None
          outerArgs.foldLeft(empty) {
            case (None, thisArg) => {
              if(thisArg.isInstanceOf[Operation]) {
                simplifyInnerOperation(thisArg.asInstanceOf[Operation])
              } else {
                None
              }
            }

            case (simplifiedTerm@Some(_), t) => {
              logger.debug(s"Ignoring argument $t of Operation $outer " +
                s"because we've already produced a simplified term $simplifiedTerm ")
              simplifiedTerm
            }
          }
        }


        case _ => None
      }

      h match {
          //handle commutative operations in the outer term
        case z: CommutativeOperation =>
          z.permutations
            //return the first permutation that simplifies
            .foldLeft(None: Option[Term]) {
              case (None, t) => simplify(t)
              case (Some(x), _) => Some(x)
            }
            .map(Right(_))
            .getOrElse(Right(h))
        case _ => simplify(h).map(Right(_)).getOrElse(Right(h))
      }

    }
  }
}
