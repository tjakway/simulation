package com.jakway.term.interpreter

import com.jakway.term.elements._
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}
import Eval._
import com.jakway.term.Util
import com.jakway.term.interpreter.helpers.{EvalFunctionCall, EvalHelpers}

/**
  * type for values we've looked up in the current evaluation context
  * @param value
  */
case class Raw[N <: NumericType[M], M](value: M)
  extends NumericTerm[N, M]
  with InterpreterResult {
  override def contains(t: Term): Boolean = equals(t)

  override def matches(other: Term): Boolean = {
    sameType(other) && value == other.asInstanceOf[Raw[N, M]].value
  }

  /**
    * print the underlying value
    * @return
    */
  override def formatResult(): String = value.toString
}

object Raw {
  def raw[N <: NumericType[M], M](e: Either[SimError, M]):
    EvalType = e.map(Raw(_))
}

/**
  *
  * @param n the interpreter takes an instance of the numeric type
  *          used to do actual calculations
  * @tparam N
  * @tparam M
  */
class Eval[N <: NumericType[M], M](val n: NumericType[M])
                                  (val evalHelpers: EvalHelpers[N, M])
  extends Interpreter {
  import Interpreter._

  def readLiteral(l: Literal[N, M]): Either[SimError, Raw[N, M]] = l match {
    case Literal(value) => n.readLiteral(value).map(Raw.apply)
  }

  def isResult(t: Term): Boolean = t match {
    case _: InterpreterResult => true
    case _ => false
  }

  def isSimplified(t: Term): Boolean = t match {
    case _: Raw[N, M] => true
    case IdentityFunction => true
    case _ => false
  }

  def allSimplified(ts: Seq[Term]) =
    ts.forall(isSimplified)

  def containsRaw(ts: Seq[Term]): Boolean =
    ts.find(_.isInstanceOf[Raw[N, M]]).isDefined


  def eval(table: SymbolTable)(t: Term): EvalType = {
    def recurse(t: Term): EvalType = eval(table)(t)

    val res: EvalType = t match {
      case l: Literal[N, M] => readLiteral(l)

      case v@Variable(name, _) => table.get(name) match {
        case Some(t) => eval(table)(t)
        case None => Left(new SymbolNotFoundError(v, table, t))
      }

      case h: HasSubterms
        if h.subterms.find(_ == null).isDefined => Left(NullSubtermError(h))

        //recurse over subterms until simplified
      case h: HasSubterms
        if !allSimplified(h.subterms) => {
        Util.mapEithers[SimError, Term, Term](h.subterms, recurse)
          .map(h.newInstance)
      }
        //we can pattern match on Raw for any operation expecting numeric terms
        //for the rest of this match statement because the HasSubterms case
        //at the top should have simplified them already

        //NOTE: because isSimplified(IdentityFunction) == true,
        //any operation that doesn't restrict its arguments to NumericTerms
        //must account for that in its case statement
        //(i.e. don't just write MyOperation(Raw(x)) because you'll
        //get a MatchError)
        //for NumericFunctions/NumericOperations you don't have to worry about
        //this because it's a compilation error to pass a non-NumericTerm
        //e.g. Add(IdentityFunction, Literal("2")) doesn't compile

      case Negative(arg) => eval(table)(Multiply(arg, Literal("-1")))

      case f: FunctionCall[N, M] => evalHelpers.functionCall(table)(f)

      case l: Logarithm[N, M] => evalHelpers.logarithm(table)(l)



      case b: BinaryNumericOperation[N @unchecked, M @unchecked] =>
        evalHelpers.binaryNumericOperation(table)(b)

      case f: TrigFunction[N @unchecked, M @unchecked] =>
        evalHelpers.trigFunctions(table)(f)

      case x: Raw[N, M] => Right(x)

        //if all subterms are simplified and we haven't matched this operation
        //then we haven't implemented it
      case h: HasSubterms
        if allSimplified(h.subterms) => Left(NotImplementedError(h))

      case x => Left(NotImplementedError(x))
    }

    res match {
      case Right(x) if !isSimplified(x) => recurse(x)
      case _ => res
    }
  }
}

object Eval {
  type EvalType = Either[SimError, Term]

  class EvalError(override val msg: String)
    extends SimError(msg)

  case class NotImplementedError(t: Term)
    extends EvalError(s"eval not implemented for term $t")

  case class ExpectedNumericTerm(override val msg: String)
    extends EvalError(msg)

  case class NullSubtermError(h: HasSubterms)
    extends EvalError(s"$h has a null subterm")

  def lookupTerm[N <: NumericType[M], M]
            (table: SymbolTable, variable: Variable[N, M]): Option[Term] =
    table.get(variable.name)

  abstract class EvalHelper[A <: Term](val interpreter: Interpreter)
  {
    def apply(table: SymbolTable)(a: A): EvalType

    def recurse(table: SymbolTable)(t: Term): EvalType =
      interpreter.eval(table)(t)
  }


  def expectNumericTerm[N <: NumericType[M], M](msg: String, t: Term):
    Either[SimError, NumericTerm[N, M]] =
    if(t.isInstanceOf[NumericTerm[N, M]]) {
      Right(t.asInstanceOf[NumericTerm[N, M]])
    } else {
      Left(ExpectedNumericTerm(s"$msg but got $t"))
    }
}
