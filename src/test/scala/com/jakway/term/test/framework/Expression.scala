package com.jakway.term.test.framework

import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.numeric.types.NumericType

/**
  * variables and term are fields and not constructor parameters
  * so that other fields can be used in their definition
  * (allowing you to define variables inside the class
  * that can be referenced in term)
 *
  * @tparam N
  * @tparam M
  */
abstract class Expression[N <: NumericType[M], M] {
  val variables: Seq[Variable[N, M]]
  val term: Term
}
