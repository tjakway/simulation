package com.jakway.term

import com.jakway.term.numeric.types.{NumericType, SimError}

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
      val toSearch: Term = equation.left

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
            }
          }
        }
      }
      ???
    }
  }

  object SubstituteFunction {
    sealed trait SubstituteFunctionInput
    case class ApplyToTerm(x: Term)
    case object ApplyToEquation


    def mkSubstituteFunctions(toReplace: Term, parent: HasSubterms,
                              identity: Term): Seq[SubstituteFunction] = {
      val newTerm = patchSubterms(parent.subterms, toReplace, identity)
      val applyToEquation =
        (orig: Seq[Term]) => {
          val keepIndex = parent.subterms.indexOf(toReplace)
          val toKeep = parent.subterms(keepIndex)
          val (left, right) = parent.subterms.splitAt(keepIndex)
          val newArgs = left ++ Seq(toKeep) ++ right
          parent.newInstance(newArgs)
        }
    }

    def applyFunctions(fs: Seq[SubstituteFunction]) = {
      fs.map { x => x.toApplyTo match {
        case  ApplyToTerm(t) => {

        }
        case ApplyToEquation => ???
      }
      }
      //patchSubterms(parent.subterms, x, replaceWith)
    }
  }


  class SubstituteFunction
          (val toApplyTo: SubstituteFunction.SubstituteFunctionInput,
           val replaceWith: Term) {
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

