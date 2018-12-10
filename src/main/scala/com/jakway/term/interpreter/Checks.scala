package com.jakway.term.interpreter

import com.jakway.term.elements.Equation
import com.jakway.term.interpreter.warn.Warning
import com.jakway.term.numeric.errors.SimError


object Checks {
  private type Check[T] = T => Seq[Warning]
  private trait HasChecks[T] {
    val checks: Seq[T => Seq[Warning]]
  }

  private def foldChecks[T](on: T,
                    checks: Seq[T => Seq[Warning]]):
    Seq[Warning] = {

    checks.foldLeft(Seq(): Seq[Warning]) {
      case (acc, thisCheck) => acc ++ thisCheck(on)
    }
  }

  private def foldChecks[T](on: T,
                    hasChecks: HasChecks[T]): Seq[Warning] =
    foldChecks(on, hasChecks.checks)

  private object EquationChecks extends HasChecks[Equation] {
    //TODO
    def checkNoVariables(eq: Equation): Seq[Warning] = ???

    val checks: Seq[Equation => Seq[Warning]] = Seq(
      checkNoVariables
    )
  }

  def checkEquation(eq: Equation): Seq[Warning] = {
    foldChecks(eq, EquationChecks)
  }
}
