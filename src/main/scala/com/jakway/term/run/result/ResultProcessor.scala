package com.jakway.term.run.result

import com.jakway.term.run.SimulationRun

trait ResultProcessor[A] {
  def apply(results: SimulationRun.RunResultType): A
}
