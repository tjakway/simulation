package com.jakway.term.solver

import com.jakway.term._
import com.jakway.term.elements._
import com.jakway.term.interpreter.warn.Warning
import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.solver.SubstituteFunction.Applications

import scala.util.{Failure, Success, Try}

class Solver[N <: NumericType[M], M] {
  import Solver._

  private def getLeftHasSubterms(e: Equation)
    : Either[SimError, HasSubterms] = e.left match {
    case x: HasSubterms => Right(x)
    case _ => Left(new SolverError(
        s"Expected instance of HasSubterms for Equation.left but" +
        s" got ${e.left}"))
  }

  private def checkEquationSolved(solveFor: Variable[N, M],
                                  applications: Applications):
    Either[SimError, Equation] = {
    val eq = applications.result
    if(eq.left matches solveFor) {
      Right(eq)
    } else {
      Left(new SolverError(s"Equation $eq not solved:" +
        s" $solveFor still present on the left side"))
    }
  }


  def solve(solveFor: Variable[N, M])
           (equation: Equation): Either[SimError, Equation] = {
    if(!equation.contains(solveFor)) {
      Left(TermNotFoundError(solveFor, equation))

      //check if the equation is already in the desired form
    } else if(equation.left.matches(solveFor)) {
      Right(equation)
    } else {
      for {
        leftSide <- getLeftHasSubterms(equation)
        functions <- SubstituteFunction.mkSubstituteFunctions(
          solveFor, leftSide)
        applications <-
          SubstituteFunction.applyFunctions(functions, equation)
        solvedEquation <- checkEquationSolved(solveFor, applications)
      } yield {
        solvedEquation
      }
    }
  }
}

object Solver {
  class SolverError(override val msg: String)
    extends SimError(msg)

  case class ImplementationError(override val msg: String)
    extends SolverError(msg)

  case class TermNotFoundError(refactorFor: Term, equation: Equation)
    extends SolverError(s"Could not find term ${refactorFor} in equation $equation")

  case class NotImplementedError(override val msg: String)
    extends SolverError(msg)

  case class PatchSubtermsError(override val msg: String)
    extends SolverError(msg)

  /**
    * replace the term we found with the identity operation for its parent
    * Note: only replaces the first element of toReplace in the passed Seq
    * @param in
    * @param toReplace
    * @param replaceWith
    * @return
    */
  def patchSubterms(in: Seq[Term], toReplace: Term, replaceWith: Term):
  Either[SimError, Seq[Term]] = {

    val errPrefix = s"Error patching Seq[Term] $in " +
      s"(replacing $toReplace with $replaceWith): "

    val numToReplace: Int = 1
    val replaceIndex = in.indexOf(toReplace)
    if(replaceIndex >= in.length || replaceIndex < 0) {
      Left(PatchSubtermsError(errPrefix +
        s"replaceIndex out of bounds (replaceIndex == $replaceIndex, " +
        s"expected < ${in.length} && >0): this likely means" +
        s" the term you want to replace ($toReplace) is not in " +
        s"the provided list ($in)"))
    } else {
      Right(in.patch(replaceIndex, Seq(replaceWith), numToReplace))
    }
  }

  def castToHasIdentity[N <: NumericType[M], M](parent: HasSubterms):
      Either[SimError, NumericOperation[N, M]] = {
    try {
      Right(parent.asInstanceOf[NumericOperation[N, M]])
    } catch {
      case x: ClassCastException => {
        val e = NotImplementedError("Currently only support refactoring " +
          "when the parent of the variable is an instance of NumericOperation")
        e.addSuppressed(x)
        Left(e)
      }
    }
  }
}

