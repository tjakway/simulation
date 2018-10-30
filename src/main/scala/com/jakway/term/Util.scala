package com.jakway.term

object Util {

  def mapLeft[L, R, A](e: Either[L, R])(f: L => A): Either[A, R]
    = e match {
    case Right(x) => Right(x)
    case Left(x) => Left(f(x))
  }

  /**
    * fail early on Left
    * @param xs
    * @param f
    * @tparam L
    * @tparam R
    * @tparam A
    * @return
    */
  def mapEithers[L, R, A](xs: Seq[A], f: A => Either[L, R])
    : Either[L, Seq[R]] = {

    val empty: Either[L, Seq[R]] = Right(Seq())
    xs.foldLeft(empty) {
      case (Right(acc), next) =>
        f(next) match {
          case Right(x) => Right(acc :+ x)
          case Left(err) => Left(err)
        }

      case (e@Left(_), _) => e
    }
  }
}
