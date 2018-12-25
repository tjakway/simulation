package com.jakway.term.run.result

import com.jakway.term.numeric.errors.SimError

import scala.concurrent.{ExecutionContext, Future}

trait GenericResultProcessor[A, B] {
  def apply(results: A)(implicit ec: ExecutionContext): Future[Either[SimError, B]]
}
