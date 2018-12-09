package com.jakway.term.test.framework.gen

import com.jakway.term.elements.{Literal, NumericTerm, Variable}
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.types.NumericType
import org.scalacheck.Arbitrary

/**
  * Arbitrary instances to accompany GenTerm
  * @param numericType
  * @tparam N
  * @tparam M
  */
trait ArbitraryTermInstances[N <: NumericType[M], M]
  extends GenTerm[N, M] {

  implicit val arbVariable: Arbitrary[Variable[N, M]] =
    Arbitrary(genVariable)

  implicit val arbM: Arbitrary[M] =
    Arbitrary(genM)

  implicit val arbRaw: Arbitrary[Raw[N, M]] =
    Arbitrary(genRaw)

  implicit val arbLiteral: Arbitrary[Literal[N, M]] =
    Arbitrary(genLiteral)

  implicit val arbNumericTerm: Arbitrary[NumericTerm[N, M]] =
    Arbitrary(genNumericTerm())
}
