package com.jakway.term.numeric.errors

case class LogarithmDomainError(base: String, of: String)
  extends SimError(s"Error calculating log_$base($of): $of <= 0 ")