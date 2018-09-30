package com.jakway.term.numeric.errors

import com.jakway.term.numeric.types.SimError

case class DivideByZeroError[M](numerator: M, denominator: M)
  extends SimError(s"Divide by zero error with numerator=$numerator and denominator=$denominator")
