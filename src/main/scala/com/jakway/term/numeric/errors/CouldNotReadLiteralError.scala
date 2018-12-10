package com.jakway.term.numeric.errors

case class CouldNotReadLiteralError(x: String)
  extends SimError(s"Could not read the string $x as a literal value")
