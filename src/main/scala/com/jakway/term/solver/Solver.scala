package com.jakway.term.solver

import com.jakway.term.elements._
import com.jakway.term.numeric.types.{NumericType, SimError}
import com.jakway.term.solver.SubstituteFunction.Applications
import org.slf4j.{Logger, LoggerFactory}

class Solver[N <: NumericType[M], M] {
  import Solver._

  val logger: Logger = LoggerFactory.getLogger(getClass())

  /**
    * move the variable we're solving for to the left side
    * @param solveFor
    * @param e
    * @return
    */
  private def moveVariableToLeft(solveFor: Variable[N, M],
                                 e: Solvable):
    Either[SimError, Solvable] = {
    if(!e.sideToSimplify.contains(solveFor) &&
        e.otherSide.contains(solveFor)) {
      Right(e.reverse())
    } else if(e.sideToSimplify.contains(solveFor)) {
      Right(e)
    } else {
      Left(TermNotFoundError(solveFor, e))
    }
  }

  private def checkSolvableSolved(solveFor: Variable[N, M],
                                  applications: Applications):
    Either[SimError, Solvable] = {
    val eq = applications.result
    if(eq.sideToSimplify matches solveFor) {
      Right(eq)
    } else {
      Left(new SolverError(s"Equation $eq not solved:" +
        s" $solveFor still present on the left side"))
    }
  }


  private def solveIfHasSubterms(solveFor: Variable[N, M],
                                 solvable: Solvable):
    Either[SimError, Solvable] = solvable.sideToSimplify match {
    case h: HasSubterms => {
      for {
        functions <- SubstituteFunction.mkSubstituteFunctions(
          solveFor, h)
        applications <-
          SubstituteFunction.applyFunctions(functions, solvable)
        solvedEquation <- checkSolvableSolved(solveFor, applications)
      } yield {
        solvedEquation
      }
    }
    case v: Variable[N @unchecked, M @unchecked]
      if solveFor == v => {
        logger.debug(s"$solvable is already solved")
        Right(solvable)
    }
    case _ => {
      Left(new SolverError("Unknown sideToSimplify in Solvable " +
        s"$solvable"))
    }
  }

  def solve(solveFor: Variable[N, M])
           (solvable: Solvable): Either[SimError, Solvable] = {
    if(!solvable.contains(solveFor)) {
      Left(TermNotFoundError(solveFor, solvable))

      //check if the equation is already in the desired form
    } else if(solvable.sideToSimplify.matches(solveFor)) {
      Right(solvable)
    } else {
      for {
        preparedSolvable <- moveVariableToLeft(solveFor, solvable)
        res <- solveIfHasSubterms(solveFor, preparedSolvable)
      } yield {
        res
      }
    }
  }
}

object Solver {
  class SolverError(override val msg: String)
    extends SimError(msg)

  case class ImplementationError(override val msg: String)
    extends SolverError(msg)

  case class TermNotFoundError(refactorFor: Term, solvable: Solvable)
    extends SolverError(s"Could not find term ${refactorFor} in solvable $solvable")

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

