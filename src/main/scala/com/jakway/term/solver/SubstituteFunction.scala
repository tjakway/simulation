package com.jakway.term.solver

import com.jakway.term.interpreter.warn.Warning
import com.jakway.term._
import com.jakway.term.numeric.types.SimError
import com.jakway.term.simplifier.{InverseIdentitySimplifier, Simplifier}
import com.jakway.term.solver.SubstituteFunction.Applications.ApplicationType

import scala.util.{Failure, Success, Try}

sealed trait SubstituteFunction

/**
  * Note that since Terms are immutable we actually apply the function
  * to the parent of the Variable and create a copy that doesn't contain
  * the term we want to replace
  * @param replaceTerm
  * @param replaceWith
  */
case class ApplyToTerm(
                        replaceTerm: Term, replaceWith: Term => Either[SimError, Term])
  extends SubstituteFunction
case class ApplyToEquation(f: Term => Term) extends SubstituteFunction

case class ApplyInverses(inverse: Term => Either[SimError, Term],
                         simplify: Simplifier) extends SubstituteFunction


object InvertFor {
  case class BadOperationError(o: Operation)
    extends SimError(s"Operation $o has subterms == Seq()")

  def apply(invertingFor: Term, z: Operation):
    Either[SimError, Term => Either[SimError, Term]] = z match {
    case o@Operation(xs) if xs.length == 1 => Right {
      (y: Term) => o.inverse(Seq(y))
    }
    case z@Operation(xs) if xs.length > 1 => Right {
      (y: Term) => Solver.patchSubterms(xs, invertingFor, y)
          .flatMap(z.inverse)
    }
    case o@Operation(Seq()) => Left(BadOperationError(o))
  }
}


object SubstituteFunction {
  case class NotOperationError(t: Term)
    extends SubstituteFunctionError(
      s"Expected passed term $t to be an instance of Operation")

  def mkSubstituteFunctions(toReplace: Term, top: HasSubterms,
                            identity: Term): Either[SimError, Seq[SubstituteFunction]] = {

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

  class Applications(val applications: Seq[Applications.ApplicationType],
                     val start: Equation,
                     val result: Equation)
  object Applications {
    type ApplicationType = (SubstituteFunction, Equation, Seq[Warning])
  }

  class SubstituteFunctionError(override val msg: String)
    extends SimError(msg) {
    def this(f: SubstituteFunction, e: Equation, msg: String) =
      this(s"Error while processing $f in $e: $msg")
  }

  case class MultipleTermsMatchSubstitutionFunction(f: SubstituteFunction,
                                                    in: Equation,
                                                    matchingTerms: Seq[Term])
    extends Warning(in, "Multiple terms matched substitution function" +
      s" $f in equation $in: $matchingTerms")

  class CouldNotFindTermError(f: ApplyToTerm,
                              in: Equation)
    extends SubstituteFunctionError(f, in,
      s"Could not find term ${f.replaceTerm} in $in")

  class ReturnedError(f: SubstituteFunction,
                      e: Equation,
                      res: SimError)
    extends SubstituteFunctionError(f, e, s"substitute function returned " +
      s"error result $res")

  class UnknownError(f: SubstituteFunction,
                     e: Equation,
                     t: Throwable)
    extends SubstituteFunctionError(f, e, s"exception thrown while processing " +
      s"substitute function: $t")

  case class SubstituteFunctionErrors(errors: Seq[SimError])
    extends SimError(s"Substitute Function errors: $errors")


  def applyFunctions(fs: Seq[SubstituteFunction], origEquation: Equation):
  Either[SimError, Applications] = {

    def applyThisFunction(f: SubstituteFunction, to: Equation):
    Either[SimError, ApplicationType] = Try {

      var matchingTerms: Seq[Term] = Seq()
      val resultingEquation: Equation = f match {
        case ApplyToTerm(replaceTerm, replaceWith) => {

          //TODO: traversing the entire AST for each substitution function
          //is very inefficient
          val newLeftTerm = TermOperations.mapAll(to.left) { thisTerm =>
            if(thisTerm == replaceTerm) {
              val substitutePerformed = matchingTerms.length > 0

              matchingTerms = matchingTerms :+ thisTerm
              if(!substitutePerformed) {
                //TODO: refactor into something more elegant than
                //throwing the error here and catching it in the Try block
                replaceWith(thisTerm).right.get
              } else {
                thisTerm
              }
            } else {
              thisTerm
            }
          }

          to.copy(left = newLeftTerm)
        }
        //apply the inverse operation to the right side of the equation
        case ApplyToEquation(g) => {
          to.copy(right = g(to.right))
        }
      }

      val warnings: Seq[Warning] =
        if(matchingTerms.length > 1) {
          Seq(MultipleTermsMatchSubstitutionFunction(f, to, matchingTerms))
        } else {
          Seq()
        }

      //return the result of the function application and
      // any warnings we've gathered from it
      (f, resultingEquation, warnings)
    } match {
      case Success(x) => Right(x)
      case Failure(e) if e.isInstanceOf[SimError] =>
        Left(new ReturnedError(f, to, e.asInstanceOf[SimError]))
      case Failure(e) => Left(new UnknownError(f, to, e))
    }


    //TODO: could add an Equation to Left and keep running after errors (to accumulate
    //more errors)
    //Left: accumulate errors
    //Right: accumulate results and track the equation to apply the next function to
    val empty: Either[Seq[SimError], (Seq[Applications.ApplicationType], Equation)] =
      Right(Seq(), origEquation)
    fs.foldLeft(empty) {
      //accumulate function applications in the Either type
      //and stop if we get a Left
      case (Right((acc, thisEquation)), thisFunction) =>
        applyThisFunction(thisFunction, thisEquation) match {
          case Right(res) => Right(acc :+ res, res._2)
          case Left(res) => Left(Seq(res))
        }

      //don't continue on error
      case (errs@Left(_), _) => errs
    }.map {
      //wrap the result and return
      case (applications, finalEquation) =>
        new Applications(applications, origEquation, finalEquation)
      } match {
        //flatten the Seq[SimError] by wrapping it in a new type
        case Left(errs) => Left(SubstituteFunctionErrors(errs))
        case Right(x) => Right(x)
      }

  }
}

