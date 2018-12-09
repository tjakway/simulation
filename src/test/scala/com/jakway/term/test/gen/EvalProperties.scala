package com.jakway.term.test.gen

import com.jakway.term.elements.{NumericTerm, Term}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.{ArbitraryTermInstances, GenEval, HasInterpreter}
import org.scalacheck.Properties

//dont forget to import this or forAll won't work
import org.scalacheck.Prop.forAll


trait EvalProperties[N <: NumericType[M], M]
  extends ArbitraryTermInstances[N, M]
    with HasInterpreter[N, M]
    with GenEval[N, M] { this: Properties =>
  import GenEval._

  property("eval numeric terms") = forAll {
    (evalTerm: EvalTerm) =>

      interpreter.eval(evalTerm.symbolTable)(evalTerm.term).isRight
  }
}
