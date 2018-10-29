package com.jakway.term.test.framework

import com.jakway.term.Term
import org.scalatest.matchers.{MatchResult, Matcher}

trait TermMatchers {

  class MatchesTerm(val expected: Term) extends Matcher[Term] {
    override def apply(left: Term): MatchResult = {
      def mkMsg(msg: String): String =
        s"$left $msg .match() $expected"

      MatchResult(left.matches(expected),
        mkMsg("did not"), mkMsg("did"))
    }
  }

  def shouldMatchTerm(expectedTerm: Term) = new MatchesTerm(expectedTerm)

  def matchTerm(expectedTerm: Term) = new MatchesTerm(expectedTerm)
}

object TermMatchers extends TermMatchers
