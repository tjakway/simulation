package com.jakway.term.numeric.errors

class SimError(val msg: String)
  extends RuntimeException(msg) {

  def this(msg: String, t: Throwable) = {
    this(msg)
    addSuppressed(t)
  }

  def this(t: Throwable) {
    this(s"Caught throwable: $t")
  }
}
