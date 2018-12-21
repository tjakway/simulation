package com.jakway.term.test.gen

import com.jakway.term.elements.{NumericTerm, Term}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.{ArbitraryTermInstances, BasePropertiesTrait, GenEval, HasInterpreter}
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.util.matching.Regex

//dont forget to import this or forAll won't work
import org.scalacheck.Prop.forAll


trait EvalProperties[N <: NumericType[M], M]
  extends ArbitraryTermInstances[N, M]
    with HasInterpreter[N, M]
    with GenEval[N, M]
    with BasePropertiesTrait {
  this: Properties =>

  import GenEval._

  protected def preventOverflow: Boolean

  /**
    * returns false if the ArithmeticException matches
    * any of the regexes, true otherwise
    */
  trait FilterArithmeticException {
    val regexes: Seq[Regex]

    def apply(e: Exception): Boolean = {
      e match {
        case arithmeticException: ArithmeticException => {
          //see if the exception mentions underflow or overflow
          val msg = arithmeticException.getMessage()
          val anyMatchFound = regexes.find(r => r.findFirstMatchIn(msg).isDefined).isDefined
          !anyMatchFound
        }
        case _ => true
      }

    }
  }

  object UnderOrOverflow extends FilterArithmeticException {
    private def underflowRegex: Regex = new Regex("(?i).*Underflow.*")
    private def overflowRegex: Regex = new Regex("(?i).*Overflow.*")

    override val regexes: Seq[Regex] = Seq(underflowRegex, overflowRegex)
  }

  object DivisionImpossible extends FilterArithmeticException {
    override val regexes: Seq[Regex] = Seq(new Regex("""(?i).*Division impossible.*"""))
  }

  def allFilters: Exception => Boolean =
    e => UnderOrOverflow(e) && DivisionImpossible(e)


  /**
    * wrapper around EvalTerm to create a Gen for terms that won't throw
    * the errors we're filtering for
    * @param e
    */
  case class RestrictedEvalTerm(e: EvalTerm)
  def genRestrictedEvalTerm(filterExceptions: Exception => Boolean): Gen[RestrictedEvalTerm] =
    //filter for terms that throw an underflow/overflow exception
    //during eval
    genEvalTerm.filter { evalTerm =>
      try {
        interpreter.eval(evalTerm.symbolTable)(evalTerm.term)
        true
      } catch {
        case e: Exception => filterExceptions(e)
        case _: Throwable => true
      }
    }.map(RestrictedEvalTerm.apply)
  implicit val arbRestrictedEvalTerm: Arbitrary[RestrictedEvalTerm] =
    Arbitrary(genRestrictedEvalTerm(allFilters))

  def checkEval(evalTerm: EvalTerm): Boolean =
    interpreter.eval(evalTerm.symbolTable)(evalTerm.term).isRight

  property("eval numeric terms") =
    if(preventOverflow) {
      forAll { (restrictedEvalTerm: RestrictedEvalTerm) =>
        checkEval(restrictedEvalTerm.e)
      }
    } else {
      forAll {
        (evalTerm: EvalTerm) => checkEval(evalTerm)
      }
    }
}
