package com.jakway.term.run.result

import com.jakway.term.run.SimulationRun

import scala.concurrent.{ExecutionContext, Future}

trait ResultProcessor[A] {
  def apply(results: SimulationRun.RunResultType)(implicit ec: ExecutionContext): Future[A]
}
