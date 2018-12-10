package com.jakway.term.elements

import com.jakway.term.elements.HasSubterms.NewInstanceF
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType

case class Logarithm[N <: NumericType[M], M]
(val base: NumericTerm[N, M],
 val of: NumericTerm[N, M])
  extends TwoArgumentFunction[N, M](base, of) {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Power.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Logarithm[N,M]](Logarithm.apply)
}

case class Power[N <: NumericType[M], M](
  val base: NumericTerm[N, M], val exponent: NumericTerm[N, M])
  extends TwoArgumentFunction[N, M](base, exponent) {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Logarithm.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Power[N,M]](Power.apply)
}

object Power {
  def root[N <: NumericType[M], M](degree: NumericTerm[N, M], x: NumericTerm[N, M]):
    Power[N, M] = Power(x, Divide(Literal("1"), degree))

  def sqrt[N <: NumericType[M], M](x: NumericTerm[N, M]): Power[N, M] = root(Literal("2"), x)
}

class NaturalLogarithm[N <: NumericType[M], M] {
  val e: NumericTerm[N, M] = Literal("e")

  /**
    * returns a log in base e
    * @param of
    * @return
    */
  def naturalLog(of: NumericTerm[N, M]): Logarithm[N, M] =
    Logarithm(e, of)
}

object NaturalLogarithm {
  def apply[N <: NumericType[M], M]
    (of: NumericTerm[N, M]): Logarithm[N, M] =
      new NaturalLogarithm().naturalLog(of)
}
