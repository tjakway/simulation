package com.jakway.term.elements

import com.jakway.term.numeric.types.NumericType

case class Variable[N <: NumericType[M], M](name: String, description: Option[String])
  extends NumericTerm[N, M]
  with UnnestedTerm {

  /**
    * check whether these represent the same variable
    * @param other
    * @return
    */
  def sameVariable(other: Variable[N, M]) = name == other.name

  override def matches(other: Term) = {
    sameType(other) &&
      sameVariable(other.asInstanceOf[Variable[N, M]])
  }
}

object Variable {
  def apply[N <: NumericType[M], M](name: String): Variable[N, M] = new Variable[N, M](name, None)
  def apply[N <: NumericType[M], M](name: String, description: String): Variable[N, M]
    = new Variable[N, M](name, Some(description))
}