package com.jakway.term.test.framework

import org.scalatest.{FlatSpec, Matchers}

abstract class TermTest
  extends FlatSpec
    with Matchers
    with TermMatchers {
}
