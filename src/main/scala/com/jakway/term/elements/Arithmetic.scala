package com.jakway.term.elements

import com.jakway.term.elements.HasSubterms.NewInstanceF
import com.jakway.term.numeric.types.{NumericType, SimError}

case class Negative[N <: NumericType[M], M](arg: NumericTerm[N, M])
  extends OneArgumentFunction[N, M]
    with NumericOperation[N,M] with HasSubterms {
  override val subterms: Seq[Term] = Seq(arg)

  override def newInstance: NewInstanceF =
    subterms => {
      Negative(HasSubterms.assertCast[NumericTerm[N,M]](HasSubterms.assertArity(1, subterms)(0)))
    }

  override def litIdentity: Literal[N, M] = Literal("0")

  override val numArguments: Int = 1
  override val argument: NumericTerm[N, M] = arg

  /**
    * Negative is its own inverse
    * @return
    */
  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Negative.apply)
}

case class Add[N <: NumericType[M], M](
                                        override val left: NumericTerm[N, M],
                                        override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Subtract.apply)


  override def litIdentity: Literal[N, M] = Literal[N, M]("0")

  override def newInstance: NewInstanceF = mkNewInstance(Add.apply)
}

case class Subtract[N <: NumericType[M], M](
                                             override val left: NumericTerm[N, M],
                                             override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Add.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("0")

  override def newInstance: NewInstanceF = mkNewInstance(Subtract.apply)
}


case class Multiply[N <: NumericType[M], M](
                                             override val left: NumericTerm[N, M],
                                             override val right: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Divide.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("1")

  override def newInstance: NewInstanceF = mkNewInstance(Multiply.apply)
}

case class Divide[N <: NumericType[M], M](
                                           val dividend: NumericTerm[N, M],
                                           val numerator: NumericTerm[N, M])
  extends BinaryNumericOperation[N, M] {

  override val left = dividend
  override val right = numerator

  override def inverseConstructorE: Seq[Term] => Either[SimError, Term] =
    mkInverseConstructorE(Multiply.apply)

  override def litIdentity: Literal[N, M] = Literal[N, M]("1")

  override def newInstance: NewInstanceF = mkNewInstance(Divide.apply)
}

