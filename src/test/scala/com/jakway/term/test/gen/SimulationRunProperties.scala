package com.jakway.term.test.gen

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.run.SimulationRun
import com.jakway.term.test.framework.gen.{GenError, GenEval, GenSimulationRun, HasInterpreter}
import com.jakway.term.test.gen.SimulationRunProperties.GenSimulationRunError
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import scala.concurrent.ExecutionContext

trait SimulationRunProperties[N <: NumericType[M], M]
  extends HasInterpreter[N, M]
    with GenSimulationRun[N, M]
    with GenEval[N, M] {
  this: Properties =>

  def getGenSimulationRun(): Gen[SimulationRun] =
    genSimulationRun(false) match {
      case Right(x) => x
      case Left(e) => throw GenSimulationRunError(e)
    }

  implicit val arbSimulationRun: Arbitrary[SimulationRun] =
    Arbitrary(getGenSimulationRun())

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  property("execute SimulationRun without error") =
    forAll { (simulationRun: SimulationRun) =>
      SimulationRun.run(simulationRun, interpreter).isRight
  }

}

object SimulationRunProperties {
  case class GenSimulationRunError(e: SimError)
    extends GenError(s"Error in genSimulationRun: $e")
}
