package com.jakway.term.test.framework

import com.jakway.term.elements.Term
import com.jakway.term.numeric.types.SimError
import com.jakway.term.test.framework.TermMatchers.MatchesTermError
import org.scalactic.Equality
import org.scalatest.matchers.{MatchResult, Matcher}

trait TermMatchers {

  class MatchesTerm(val expected: Term) extends Matcher[Term] {
    override def apply(left: Term): MatchResult = {
      def mkMsg(msg: String): String =
        s"$left $msg .match() $expected"

      //sanity check
      checkMatchEachOther(left, expected)

      MatchResult(left.matches(expected),
        mkMsg("did not"), mkMsg("did"))
    }

    /**
      * if a.matches(b) then b.matches(a) should return true
      * and vice-versa
      */
    def checkMatchEachOther(a: Term, b: Term) = {
      if(a.matches(b)) {
        if(!b.matches(a)) {
          throw MatchesTermError(s"a.matches(b) returned true" +
            s" but b.matches(a) returned false" +
            s" for a=$a, b=$b")
        }
      }

      if(b.matches(a)) {
        if(!a.matches(b)) {
          throw MatchesTermError(s"b.matches(a) returned true" +
            s" but a.matches(b) returned false" +
            s" for a=$a, b=$b")
        }
      }
    }

  }

  def shouldMatchTerm(expectedTerm: Term) = new MatchesTerm(expectedTerm)

  def matchTerm(expectedTerm: Term) = new MatchesTerm(expectedTerm)
}

object TermMatchers extends TermMatchers {
  case class MatchesTermError(override val msg: String)
    extends SimError(msg)

  def assertMatch(actual: Term, expected: Term): MatchResult = {
    new MatchesTerm(expected).apply(actual)
  }

  def assertMatchOrThrow(actual: Term, expected: Term): Unit = {
    val res = assertMatch(actual, expected)
    if(!res.matches) {
      throw MatchesTermError(res.failureMessage)
    }
  }

  //use the MatchResult to check for equality
  val equalityInstance: Equality[Term] = new Equality[Term] {
    override def areEqual(a: Term, b: Any): Boolean = {
      def eq(x: Term, y: Term): Boolean = {
        new MatchesTerm(x).apply(y).matches
      }

      b.isInstanceOf[Term] && eq(a, b.asInstanceOf[Term])
    }
  }
}
