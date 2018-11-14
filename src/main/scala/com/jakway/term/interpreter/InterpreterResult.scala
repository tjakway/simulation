package com.jakway.term.interpreter

import com.jakway.term.elements.Term

trait InterpreterResult extends Term {
  def formatResult(): String = toString
}
