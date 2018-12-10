package com.jakway.term.test.gen

import com.jakway.term.elements.{NumericTerm, Term}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.{ArbitraryTermInstances, GenEval, HasInterpreter}
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.util.matching.Regex

//dont forget to import this or forAll won't work
import org.scalacheck.Prop.forAll


trait EvalProperties[N <: NumericType[M], M]
  extends ArbitraryTermInstances[N, M]
    with HasInterpreter[N, M]
    with GenEval[N, M] {
  this: Properties =>

  import GenEval._

  protected def preventOverflow: Boolean

  object IsUnderOrOverflow {
    private def underflowRegex: Regex = new Regex("(?i).*Underflow.*")
    private def overflowRegex: Regex = new Regex("(?i).*Overflow.*")

    def apply(e: Exception): Boolean = {
      e match {
        case arithmeticException: ArithmeticException => {
          //see if the exception mentions underflow or overflow
          val msg = arithmeticException.getMessage()
          underflowRegex.findFirstMatchIn(msg).isDefined ||
            overflowRegex.findFirstMatchIn(msg).isDefined
        }
        case _ => false
      }
    }
  }


  /**
    * wrapper around EvalTerm to create a Gen for terms that won't throw
    * underflow or overflow errors
    * @param e
    */
  case class RestrictedEvalTerm(e: EvalTerm)
  def genRestrictedEvalTerm: Gen[RestrictedEvalTerm] =
    //filter for terms that throw an underflow/overflow exception
    //during eval
    genEvalTerm.filter { evalTerm =>
      try {
        interpreter.eval(evalTerm.symbolTable)(evalTerm.term)
        true
      } catch {
        case e: Exception if IsUnderOrOverflow(e) => false
        case _: Throwable => true
      }
    }.map(RestrictedEvalTerm.apply)
  implicit val arbRestrictedEvalTerm: Arbitrary[RestrictedEvalTerm] =
    Arbitrary(genRestrictedEvalTerm)

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
