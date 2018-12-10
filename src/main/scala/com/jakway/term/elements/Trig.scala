package com.jakway.term.elements

import com.jakway.term.elements.HasSubterms.NewInstanceF
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType

trait TrigFunction[N <: NumericType[M], M]
  extends OneArgumentFunction[N, M]

case class Sin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arcsin.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Sin[N,M]](Sin.apply)
}
case class Cos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arccos.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Cos[N,M]](Cos.apply)
}
case class Tan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Arctan.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Tan[N,M]](Tan.apply)
}
case class Arcsin[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Sin.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arcsin[N,M]](Arcsin.apply)
}
case class Arccos[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Cos.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arccos[N,M]](Arccos.apply)
}
case class Arctan[N <: NumericType[M], M](override val argument: Term) extends TrigFunction[N, M] {
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Tan.apply)

  override def newInstance: NewInstanceF = mkNewInstance[Arctan[N,M]](Arctan.apply)
}
