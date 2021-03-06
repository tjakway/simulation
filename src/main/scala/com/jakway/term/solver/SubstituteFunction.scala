package com.jakway.term.solver

import com.jakway.term.interpreter.warn.Warning
import com.jakway.term._
import com.jakway.term.elements.{Equation, HasSubterms, Operation, Term}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.simplifier.{InverseIdentitySimplifier, Simplifier}
import com.jakway.term.solver.SubstituteFunction.Applications.{Inversion, Simplification}
import interface.Formatter

import scala.util.{Failure, Success, Try}

sealed trait SubstituteFunction

case class ApplyInverses(inverse: Term => Either[SimError, Term],
                         simplifier: Simplifier) extends SubstituteFunction


object InvertFor {
  case class BadOperationError(o: Operation)
    extends SimError(s"Operation $o has subterms == Seq()")

  case class VariableNotFoundError(t: Term, in: Seq[Term])
    extends SimError(s"Could not find $t either " +
      s"as an element or child of $in")

  def apply(invertingFor: Term, z: Operation):
    Either[SimError, Term => Either[SimError, Term]] = z match {
    case o@Operation(xs) if xs.length == 1 => Right {
      (y: Term) => o.inverse(Seq(y))
    }
    case z@Operation(xs) if xs.length > 1 => {
      //patch whichever subterm contains the term we're inverting for
        val subtermToPatch =
          xs.find(TermOperations.containsTerm(invertingFor, _)) match {
            case Some(found) => Right(found)
            case None => Left(VariableNotFoundError(invertingFor, xs))
          }

        subtermToPatch
          .map(toReplace =>
              (y: Term) => Solver.patchSubterms(xs, toReplace, y)
                                      .flatMap(z.inverse))

    }
    case o@Operation(Seq()) => Left(BadOperationError(o))
  }
}


object SubstituteFunction {
  case class NotOperationError(t: Term)
    extends SubstituteFunctionError(
      s"Expected passed term $t to be an instance of Operation")

  def mkSubstituteFunctions(toReplace: Term, top: HasSubterms):
    Either[SimError, Seq[SubstituteFunction]] = {

    val empty: Either[Seq[SimError], Seq[SubstituteFunction]] = Right(Seq())

    val res = TermOperations
      .parentsOf(toReplace, top)
      //reverse the list of parents so it's
      //top -> bottom
      //aka outermost term -> innermost term
      .reverse
      .foldLeft(empty) {
        case (Right(fs), x@Operation(_)) => {
          val res = InvertFor(toReplace, x)
            .map(i => ApplyInverses(i, new InverseIdentitySimplifier()))
            .map(i => fs :+ i)

          Util.mapLeft(res)(e => Seq(e))
        }


        case (Left(acc), x) => Left(acc :+ NotOperationError(x))
        case (_, x) => Left(Seq(NotOperationError(x)))
      }

    Util.mapLeft(res)(SubstituteFunctionErrors(_))
  }

  class Applications(val applications: Seq[Applications.Application],
                     val start: Solvable,
                     val result: Solvable)
  object Applications {
    case class Simplification(left: Term, right: Term)
    case class Inversion(left: Term, right: Term)

    case class Application(subF: SubstituteFunction,
                           inversion: Inversion,
                           simplification: Simplification,
                           result: Solvable,
                           warnings: Seq[Warning] = Seq())
  }
  import Applications._

  class SubstituteFunctionError(override val msg: String)
    extends SimError(msg) {
    def this(f: SubstituteFunction, e: Solvable, msg: String) =
      this(s"Error while processing $f in $e: $msg")
  }

  case class MultipleTermsMatchSubstitutionFunction(f: SubstituteFunction,
                                                    in: Solvable,
                                                    matchingTerms: Seq[Term])
    extends Warning(in, "Multiple terms matched substitution function" +
      s" $f in equation $in: $matchingTerms")

  class ReturnedError(f: SubstituteFunction,
                      e: Solvable,
                      res: SimError)
    extends SubstituteFunctionError(f, e, s"substitute function returned " +
      s"error result $res")

  class UnknownError(f: SubstituteFunction,
                     e: Solvable,
                     t: Throwable)
    extends SubstituteFunctionError(f, e, s"exception thrown while processing " +
      s"substitute function: $t")

  case class SubstituteFunctionErrors(errors: Seq[SimError])
    extends SimError(
      new Formatter()
        .formatSeqMultiline("Substitute Function errors:")(errors))


  def applyFunctions(fs: Seq[SubstituteFunction], origSolvable: Solvable):
  Either[SimError, Applications] = {

    def applyThisFunction(subF: SubstituteFunction, to: Solvable):
    Either[SimError, Application] = Try {

      val result: Either[SimError, Application] =
        subF match {
        case s@ApplyInverses(inv, simplifier) => {
          def f(t: Term): Either[SimError, Term] =
            inv(t).flatMap(simplifier.simplify)

          //make sure we actually simplified something
          def changed(msg: String)(invL: Term, invR: Term): Either[SimError, Unit] = {
            case class ExpectedSimplificationError(override val msg: String)
              extends SimError(msg)

            if(invL == to.sideToSimplify && invR == to.otherSide) {
              Left(ExpectedSimplificationError(
                s"Expected applying SimplificationFunction $s" +
                  s" would result in a different equation; " + msg))
            } else {
              Right(())
            }
          }

          for {
            invLeft <- inv(to.sideToSimplify)
            invRight <- inv(to.otherSide)
            _ <- changed("inversion step changed nothing")(invLeft, invRight)

            sLeft <- simplifier.simplify(invLeft)
            sRight <- simplifier.simplify(invRight)
            _ <- changed("simplification step changed nothing")(sLeft, sRight)
          } yield {
            val newEq = Solvable(sLeft,sRight)

            Application(subF, Inversion(invLeft, invRight), Simplification(sLeft, sRight),
              newEq, Seq())
          }
        }
      }

      result
    } match {
      case Success(x) => x
      case Failure(e) if e.isInstanceOf[SimError] =>
        Left(new ReturnedError(subF, to, e.asInstanceOf[SimError]))
      case Failure(e) => Left(new UnknownError(subF, to, e))
    }


    //TODO: could add an Equation to Left and keep running after errors (to accumulate
    //more errors)
    //Left: accumulate errors
    //Right: accumulate results and track the equation to apply the next function to
    val empty: Either[Seq[SimError], (Seq[Applications.Application], Solvable)] =
      Right(Seq(), origSolvable)
    fs.foldLeft(empty) {
      //accumulate function applications in the Either type
      //and stop if we get a Left
      case (Right((acc, thisEquation)), thisFunction) =>
        applyThisFunction(thisFunction, thisEquation) match {
          case Right(res) => Right(acc :+ res, res.result)
          case Left(res) => Left(Seq(res))
        }

      //don't continue on error
      case (errs@Left(_), _) => errs
    }.map {
      //wrap the result and return
      case (applications, finalEquation) =>
        new Applications(applications, origSolvable, finalEquation)
      } match {
        //flatten the Seq[SimError] by wrapping it in a new type
        case Left(errs) => Left(SubstituteFunctionErrors(errs))
        case Right(x) => Right(x)
      }

  }
}

