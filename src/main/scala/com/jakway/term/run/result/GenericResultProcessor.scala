package com.jakway.term.run.result

import com.jakway.term.numeric.errors.SimError

import scala.concurrent.{ExecutionContext, Future}

trait GenericResultProcessor[A, B] {
  def apply(results: A)(implicit ec: ExecutionContext): Future[Either[SimError, B]]
}

object GenericResultProcessor {
  def mapResults[A, B](res: Future[Either[SimError, A]],
                       f: A => Future[Either[SimError, B]])
                      (implicit ec: ExecutionContext): Future[Either[SimError, B]] =
    res.flatMap { x =>
      x match {
        case Right(y) => f(y)
        case Left(t) => Future(Left(t))
      }
    }
}