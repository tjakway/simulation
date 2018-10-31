package com.jakway.term

import com.jakway.term.elements.{HasSubterms, Operation, Term, Variable}
import com.jakway.term.numeric.types.{NumericType, SimError}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object TermOperations {
  case class CastHasSubtermsError(castTerm: Term, t: Throwable)
    extends SimError(s"Error casting $castTerm to an instance of HasSubterms",
      t)

  val logger: Logger = LoggerFactory.getLogger(getClass())

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

  def foldSubterms[B](t: Term)(z: B)(f: (B, Term) => B): B = {
    t match {
      //fold over the top level term then subterms
      case h: HasSubterms => {
        val start = f(z, h)
        h.subterms.foldLeft(start) {
          case (acc, thisTerm) => foldSubterms(thisTerm)(acc)(f)
        }
      }
      case _ => f(z, t)
    }
  }

  def findVariables[N <: NumericType[M], M](t: Term)
    : Seq[Variable[N, M]] = {

    val empty: Seq[Variable[N, M]] = Seq()

    //accumulate variables
    foldSubterms(t)(empty) {
      case (variables, x) => {

        //need to check reified types to get around type erasure
        if(x.isInstanceOf[Variable[N @unchecked, M @unchecked]]) {
          variables :+ x.asInstanceOf[Variable[N, M]]
        }
        else {
          variables
        }
      }
    }
  }

  def findParentOf(term: Term, in: HasSubterms): Option[HasSubterms] = {
    foldSubterms(in)(None: Option[HasSubterms]) {
      case (None, x@HasSubterms(subterms))
          if subterms.contains(term) => Some(x)

        //ignore other branches
      case (None, _) => None

        //stop once we've found the parent
      case (x@Some(_), _) => x
    }
  }

  /**
    * returns all the parent nodes of the passed term
    * in order from closest -> farthest
    * (i.e. bottom to top)
    * @param term
    * @param in
    * @return
    */
  def parentsOf(term: Term, in: HasSubterms): Seq[HasSubterms] = {
    if(term == in) {
      Seq()
    } else {
      /**
        * tail-recursive helper function to walk up the tree
        * @param acc
        * @param toFind
        * @return
        */
      @tailrec
      def helper(acc: Seq[HasSubterms])(toFind: Term): Seq[HasSubterms] = {
        findParentOf(toFind, in) match {
            //stop when we reach the top of the tree
          case None => {
            assert(toFind == in)
            acc
          }
            //recurse up
          case Some(next) => helper(acc :+ next)(next)
        }
      }

      helper(Seq())(term)
    }
  }

  /**
    * contains but using .matches instead of ==
    * @param x
    * @param y
    * @return
    */
  def hasMatchingMember(x: HasSubterms, y: Term): Boolean =
    x.subterms.find(_.matches(y)).isDefined

  /**
    * @param a
    * @param b
    * @return a list of subterms that aren't shared by its arguments
    */
  def getDisjointSubterms(a: HasSubterms, b: HasSubterms): Seq[Term] = {
    a.subterms.filter(!hasMatchingMember(b, _)) ++
      b.subterms.filter(!hasMatchingMember(a, _))
  }

  /**
    * Opposite of getDisjointSubterms
    * @param a
    * @param b
    * @return
    */
  def getOverlappingSubterms(a: HasSubterms, b: HasSubterms): Set[Term] = {
    (a.subterms.filter(hasMatchingMember(b, _))
      ++ b.subterms.filter(hasMatchingMember(a, _))).toSet
  }

  def invertTerm(t: Term): Either[SimError, Term] = t match {
    case o: Operation => o.inverted
    case _ => Right(t)
  }

  def containsTerm(toFind: Term, in: Term): Boolean =
    foldSubterms(in)(false) {
      case (false, x) => x matches toFind
      case (true, _) => true
    }

  def topParentOf(x: Term, in: HasSubterms): Option[Term] = {
    parentsOf(x, in).lastOption
  }
}
