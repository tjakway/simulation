package com.jakway.term.elements.util

import com.jakway.term.elements.{Operation, Term}
import com.jakway.term.numeric.errors.SimError

import scala.reflect.ClassTag

object InverseConstructorHelpers {

  /**
    * mkInverseConstructorE for 1-arity types
    *
    * takes a 1-arity constructor and returns a function that
    * will construct that type from a Seq[Term]
    *
    * see https://stackoverflow.com/questions/50790011/scala-lambda-cannot-be-cast-to-classtag-error
    * for getting a ClassTag when that generic also needs type constraints
    * @return
    */
  def arity1MkInverseConstructorE[ConstructorArgType <: Term : ClassTag]:
  (ConstructorArgType => Term) => Seq[Term] => Either[SimError, Term] = {
    ctor =>
      (args: Seq[Term]) =>
        val numArguments = 1 //arity 1 constructor
        val Seq(le) = args.take(numArguments)
        val res: Either[SimError, ConstructorArgType] =
          Operation.checkCast[ConstructorArgType](le)

        res.map(ctor)
  }


  /**
    * mkInverseConstructorE for 2-arity types
    * @return
    */
  def arity2MkInverseConstructorE[ConstructorArgType <: Term : ClassTag]:
  ((ConstructorArgType, ConstructorArgType) => Term) => Seq[Term] => Either[SimError, Term] = {
    ctor =>
      (args: Seq[Term]) =>
        val numArguments = 2
        val Seq(le, re) = args.take(numArguments)
        val res: Either[SimError, (ConstructorArgType, ConstructorArgType)] = for {
          l <- Operation.checkCast[ConstructorArgType](le)
          r <- Operation.checkCast[ConstructorArgType](re)
        } yield (l, r)

        res.map(x => ctor(x._1, x._2))
  }
}
