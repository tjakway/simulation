package com.jakway.term.elements

import java.util.UUID

trait Term {
  def contains(t: Term): Boolean

  val uniqueId: UUID = java.util.UUID.randomUUID()

  /**
    * equals ignoring UUID
    * @param other
    * @return
    */
  def matches(other: Term): Boolean

  def sameType(other: Any): Boolean =
    getClass() == other.getClass()
}
