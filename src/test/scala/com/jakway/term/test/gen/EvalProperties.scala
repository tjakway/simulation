package com.jakway.term.test.gen

import com.jakway.term.elements.NumericTerm
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.{ArbitraryTermInstances, HasInterpreter}
import org.scalacheck.Properties

//dont forget to import this or forAll won't work
import org.scalacheck.Prop.forAll


trait EvalProperties[N <: NumericType[M], M]
  extends ArbitraryTermInstances[N, M]
    with HasInterpreter[N, M] { this: Properties =>

  property("eval numeric terms") = forAll {
    (numericTerm: NumericTerm[N, M]) =>
      interpreter.eval(Map())(numericTerm).isRight
  }
}
