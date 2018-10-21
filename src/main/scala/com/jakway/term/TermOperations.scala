package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}
import org.slf4j.{Logger, LoggerFactory}

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

  def findParentOf(term: Term, in: Term): Option[HasSubterms] = {
    foldSubterms(term)(None: Option[Term]) {
      case (None, x@HasSubterms(subterms))
          if subterms.contains(term) => Some(x)
        //stop once we've found the parent
      case (x@Some(_), _) => x
    }
  }

}
