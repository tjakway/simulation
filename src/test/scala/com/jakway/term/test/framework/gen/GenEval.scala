package com.jakway.term.test.framework.gen

import com.jakway.term.TermOperations
import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.misc.BuildableSeq
import org.scalacheck.{Arbitrary, Gen}

/**
  * holds Gens related to Eval/Interpreter
 *
  * @tparam N
  * @tparam M
  */
trait GenEval[N <: NumericType[M], M]
  extends GenTerm[N, M] {
  import GenEval._

  def genSymbolTable(forTerm: Term): Gen[SymbolTable] = {
    type SeqElem = (Variable[N, M], Term)
    val seqGen: Gen[Seq[SeqElem]] =
      Gen.sequence(TermOperations
      .findVariables[N, M](forTerm)
      .map { thisVariable =>
        //cast the NumericTerm up to Term to match the type of
        //SymbolTable
        genConstantNumericTerm().map(n => (thisVariable, n: Term))
      })(BuildableSeq.buildableSeq[SeqElem])

    for {
      pairs <- seqGen
    } yield {
      pairs.map {
        case (thisVar, thisTerm) => (thisVar.name, thisTerm)
      }.toMap
    }
  }

  def genEvalTerm: Gen[EvalTerm] = {
    for {
      constantTerm <- genConstantNumericTerm()
      accompanyingSymbolTable <- genSymbolTable(constantTerm)
    } yield EvalTerm(accompanyingSymbolTable, constantTerm)
  }

  implicit val arbEvalTerm: Arbitrary[EvalTerm] = Arbitrary(genEvalTerm)
}

object GenEval {
  case class EvalTerm(symbolTable: SymbolTable, term: Term)
}
