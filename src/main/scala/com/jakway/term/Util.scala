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


  def accEithers[L, R](xs: Seq[Either[L, R]]): Either[Seq[L], Seq[R]] = {
    xs.foldLeft(Right(Seq()): Either[Seq[L], Seq[R]]) {
      case (Left(es), Left(e)) => Left(es :+ e)
      case (Right(_), Left(e)) => Left(Seq(e))
      case (Right(as), Right(a)) => Right(as :+ a)
      case (Left(es), Right(_)) => Left(es)
    }
  }

  def traversableAccEithers[CL <: TraversableOnce[L],
                            CR <: TraversableOnce[R], L, R]
      (clAppend: CL => L => CL, crAppend: CR => R => CR)
      (newCL: L => CL, newCR: R => CR)
      (empty: Either[CL, CR])
      (xs: TraversableOnce[Either[L, R]]): Either[CL, CR] = {

    xs.foldLeft(empty) {
        case (Left(es), Left(e)) => Left(clAppend(es)(e))
        case (Right(_), Left(e)) => Left(newCL(e))
        case (Right(as), Right(a)) => Right(crAppend(as)(a))
        case (Left(es), Right(_)) => Left(newCL(es))
      }
  }

  def genericAccEithers[LeftCollectionType, RightCollectionType, L, R]
    (leftTypeAppendOperation: LeftCollectionType => L => LeftCollectionType,
     rightTypeAppendOperation: RightCollectionType => R => RightCollectionType,
     empty: Either[LeftCollectionType, RightCollectionType],
     nextOperation: RightCollectionType => RightCollectionType) = {
    def accEithers[L, R](xs: Seq[Either[L, R]]): Either[Seq[L], Seq[R]] = {
      xs.foldLeft(Right(Seq()): Either[Seq[L], Seq[R]]) {
        case (Left(es), Left(e)) => Left(es :+ e)
        case (Right(_), Left(e)) => Left(Seq(e))
        case (Right(as), Right(a)) => Right(as :+ a)
        case (Left(es), Right(_)) => Left(es)
      }
    }
  }

  def appendLeftOrReplace[L, R](xs: Either[Seq[L], R], x: L): Either[Seq[L], R] = {
    xs match {
      case Left(es) => Left(es :+ x)
      case Right(_) => Left(Seq(x))
    }
  }
}
