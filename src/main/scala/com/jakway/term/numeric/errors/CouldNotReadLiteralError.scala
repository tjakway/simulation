package com.jakway.term.numeric.errors

import com.jakway.term.numeric.types.SimError

case class CouldNotReadLiteralError(x: String)
  extends SimError(s"Could not read the string $x as a literal value")
