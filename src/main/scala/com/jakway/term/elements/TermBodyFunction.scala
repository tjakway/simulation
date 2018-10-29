package com.jakway.term.elements

import com.jakway.term.TermOperations
import com.jakway.term.numeric.types.{NumericType, SimError}

/**
  * a function that doesn't require interpreter support
  * for its implementation
  */
abstract class TermBodyFunction[N <: NumericType[M], M]
            (val parameters: Seq[Variable[N, M]]) {
  import TermBodyFunction._

  def name: String = getClass().getName()

  val arity: Int = parameters.length

  def body: Term
  def call(arguments: Seq[Term]): FunctionCall[N, M] =
    FunctionCall[N, M](this, arguments)

  def call(arguments: Term*): FunctionCall[N, M] =
    call(arguments)

  def apply: Seq[Term] => FunctionCall[N, M] = call

  def inverseFunction: Either[SimError, TermBodyFunction[N, M]] = {
    val thisName = name
    TermOperations.invertTerm(body).map { invertedBody =>
      new TermBodyFunction[N, M](parameters) {
        def body: Term = invertedBody
        override def name: String = nameInverse(thisName)
      }
    }
  }
}

object TermBodyFunction {
  def nameInverse(name: String): String =
    s"Inverse[$name]"
}

case class FunctionCall[N <: NumericType[M], M]
            (function: TermBodyFunction[N, M], arguments: Seq[Term])
  extends NumericTerm[N, M] {

  def inverse: Either[SimError, FunctionCall[N, M]] = {
    function.inverseFunction
            .map(inverseF => this.copy(function = inverseF))
  }

  override def matches(other: Term): Boolean = {
    def f(o: FunctionCall[N, M]): Boolean =
      function == o.function && arguments == o.arguments

    sameType(other) && f(other.asInstanceOf[FunctionCall[N, M]])
  }

  /**
    * check if any arguments contain the passed term
    * note: don't check if the function body contains it
    * @param t
    * @return
    */
  override def contains(t: Term): Boolean = arguments.find(_.contains(t)).isDefined
}