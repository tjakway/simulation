package com.jakway.term

import com.jakway.term.interpreter.warn.Warning
import com.jakway.term.numeric.types.{NumericType, SimError}

import scala.util.{Failure, Success, Try}

class Solver[N <: NumericType[M], M] {
  case class TermNotFoundError(refactorFor: Term, equation: Equation)
    extends SimError(s"Could not find term ${refactorFor} in equation $equation")

  case class NotImplementedError(override val msg: String)
    extends SimError(msg)

  case class PatchSubtermsError(override val msg: String)
    extends SimError(msg)

  def solve(refactorFor: Variable[N, M])
           (equation: Equation): Either[SimError, Equation] = {
    if(!equation.contains(refactorFor)) {
      Left(TermNotFoundError(refactorFor, equation))

      //check if the equation is already in the desired form
    } else if(equation.left == refactorFor) {
      Right(equation)
    } else {
      //TODO XXX

      //TODO: use a case to check if equation.left is:
      // -the term we're looking for
      // -a different unnested term
      // -an instance of HasSubterms
      //and handle appropriately
      val toSearch: HasSubterms = equation.left
        .asInstanceOf[HasSubterms]

      var replacements: Seq[(Term => Term)] = Seq()

      TermOperations.mapAll(toSearch) {
        //search for variables to move to the right side
        case x: Variable[N, M] if !x.sameVariable(refactorFor) => {

          TermOperations.findParentOf(x, toSearch) match {
            case None => ??? //TODO: implement
            case Some(parent) => {
              //we're replacing the variable x with the identity
              castToHasIdentity(parent)
                .flatMap { castParent =>
                  val replaceWith = castParent.identity
                  patchSubterms(parent.subterms, x, replaceWith)
                }
              ???
            }
          }
        }
      }
      ???
    }
  }

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

  object SubstituteFunction {
    def mkSubstituteFunctions(toReplace: Term, parent: HasSubterms,
                              identity: Term): Seq[SubstituteFunction] = {

      val newTerm = { (inputToReplace: Term) =>
        patchSubterms(parent.subterms, inputToReplace, identity).map(parent.newInstance)
      }

      val applyToEquation =
        (varToReplace: Term) => {
          val replaceIndex = parent.subterms.indexOf(varToReplace)
          val toReplace = parent.subterms(replaceIndex)
          val (left, right) = parent.subterms.splitAt(replaceIndex)
          val newArgs = left ++ Seq(varToReplace) ++ right
          parent.newInstance(newArgs)
        }

      Seq(ApplyToTerm(toReplace, newTerm), ApplyToEquation(applyToEquation))
    }

    class Applications(val applications: Seq[(SubstituteFunction, Equation, Seq[Warning])],
                       val start: Equation,
                       val result: Equation)

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


    def applyFunctions(fs: Seq[SubstituteFunction], orig: Equation):
      Either[SimError, Applications] = {

      def thisApplication(f: SubstituteFunction, to: Equation):
        Either[SimError, (Equation, Seq[Warning])] = Try {

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
        (resultingEquation, warnings)
      } match {
        case Success(x) => Right(x)
        case Failure(e) if e.isInstanceOf[SimError] =>
          Left(new ReturnedError(f, to, e.asInstanceOf[SimError]))
        case Failure(e) => Left(new UnknownError(f, to, e))
      }

    }
  }


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

    val replaceIndex = in.indexOf(toReplace)
    if(replaceIndex >= in.length || replaceIndex < 0) {
      Left(PatchSubtermsError(errPrefix +
        s"replaceIndex out of bounds (replaceIndex == $replaceIndex, " +
        s"expected < ${in.length} && >0)"))
    } else {
      Right(in.patch(0, Seq(replaceWith), replaceIndex))
    }
  }

  def castToHasIdentity(parent: HasSubterms): Either[SimError, NumericOperation[N, M]] = {
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

