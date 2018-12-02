package com.jakway.term.test.framework.cases

trait NamedTestCase {
  val namePrefix: String
  def fullName: String = namePrefix + "." + name
  val name: String
}
