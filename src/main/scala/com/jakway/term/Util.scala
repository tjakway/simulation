package com.jakway.term

object Util {

  def mapLeft[L, R, A](e: Either[L, R])(f: L => A): Either[A, R]
    = e match {
    case Right(x) => Right(x)
    case Left(x) => Left(f(x))
  }
}
