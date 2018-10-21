package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

class Solver {
  case class TermNotFoundError(refactorFor: Term, equation: Equation)
    extends SimError(s"Could not find term ${refactorFor} in equation $equation")

  def solve(refactorFor: Term)(equation: Equation): Either[SimError, Equals] = {
    if(!equation.contains(refactorFor)) {
      Left(TermNotFoundError(refactorFor, equation))

      //check if the equation is already in the desired form
    } else if(equation.left == refactorFor) {
      Right(equation)
    } else {
      //TODO XXX
      TermOperations.mapAll(equation.left) {
        ???
      }
      ???
    }
  }
}

