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

  def ifNone[A](o: Option[A])(f: () => Unit): Unit = o match {
    case None => f()
    case _ => ()
  }

  /**
    * cross-product
    * modified from https://stackoverflow.com/questions/14740199/cross-product-in-scala
    */
  /*def crossJoin[T](list: Stream[Stream[T]]): Stream[Stream[T]] =
    list match {
      case xs #:: Stream() => xs map (Stream(_))
      case Stream(x) #:: xs => for {
        i <- x: Stream[T]
        j <- crossJoin(xs)
      } yield Stream(i) ++ j
    }
  */

  def crossJoin[T](list: Stream[Stream[T]]): Stream[Stream[T]] = {
    def safeTail[A](qs: Stream[A]): Stream[A] = qs.drop(1)

    list match {
      case Stream() => Stream()
      case xs #:: Stream() => xs map (Stream(_))
      case _ => {
        list.head.flatMap(r =>
          Stream(Stream[T](r) ++ crossJoin[T](safeTail[Stream[T]](list))))
          .asInstanceOf[Stream[Stream[T]]]
      }
    }
  }
}
