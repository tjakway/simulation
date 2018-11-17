package com.jakway.term.test.framework

import com.jakway.term.numeric.types.SimError

import scala.util.{Failure, Success, Try}

/**
  * exception indicating an error in test rather than application code
  * @param msg
  */
class TestError(override val msg: String)
  extends SimError(msg) {

  def this(t: Throwable) {
    this(s"Caught throwable: $t")
  }
}


object TestError {
  def wrapTry[A](ctor: Throwable => TestError)(t: Try[A]): Either[TestError, A]
    = t match {
    case Success(x) => Right(x)
    case Failure(t) => Left(ctor(t))
  }

  def wrapEx[A](ctor: Throwable => TestError)(f: => A): Either[TestError, A] = {
    try {
      Right(f)
    } catch {
      case t: Throwable => Left(ctor(t))
    }
  }
}