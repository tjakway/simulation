package com.jakway.term.run

trait ResultProcessor[A] {
  def apply(results: SimulationRun.RunResultType): A
}
