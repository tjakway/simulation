package com.jakway.term.run.result

import scala.concurrent.{ExecutionContext, Future}

trait GenericResultProcessor[A, B] {
  def apply(results: A)(implicit ec: ExecutionContext): Future[B]
}
