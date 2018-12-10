package com.jakway.term.elements

import com.jakway.term.numeric.types.{NumericType, SimError}

trait DomainRestriction[N <: NumericType[M], M, Z <: Operation] {
  def checkDomain(z: Z, numericType: N): Either[SimError, Z]
}

object DomainRestriction {
  trait Bound
  case class UpperBound(bound: String) extends Bound
  case class LowerBound(bound: String) extends Bound

  /**
    * @param violated error if violated.size <= 0
    *                 (could make DomainRestrictionError extend
    *                 DomainRestriction to enforce this but that
    *                 would require adding type parameters and
    *                 the complexity is not worth the cleverness)
    * @param fieldName
    */
  class DomainRestrictionError(override val msg: String)
    extends SimError(msg)

  object DomainRestrictionError {
    def apply[Z <: Operation](
             op: Z,
             violated: Set[Bound],
             fieldName: Option[String]): DomainRestrictionError = {

      if (violated.size <= 0) {
        throw new DomainRestrictionError (
        "Error when constructing an instance of" +
        s" DomainRestrictionError with args op=$op, " +
        s"violated=$violated, fieldName=$fieldName: " +
        s"violated.size must be >= 0")
      } else {
        new DomainRestrictionError (s"Violations for $op" +
        fieldName.map ("." + _).getOrElse ("") + ": " +
        s"$violated")
      }
    }
  }
}