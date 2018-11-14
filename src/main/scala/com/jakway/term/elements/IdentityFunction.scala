package com.jakway.term.elements

import com.jakway.term.interpreter.InterpreterResult

object IdentityFunction
  extends UnnestedTerm
  with InterpreterResult {

  override def matches(other: Term) =
    sameType(other)
}
