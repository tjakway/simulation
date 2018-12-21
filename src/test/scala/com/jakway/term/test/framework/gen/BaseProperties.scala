package com.jakway.term.test.framework.gen

import com.jakway.term.numeric.types.NumericType
import org.scalacheck.Properties
import org.slf4j.{Logger, LoggerFactory}

trait BasePropertiesTrait {
  val logger: Logger
}

abstract class BaseProperties[N <: NumericType[M], M](
  override val name: String, override val numericType: N)
  extends Properties(name)
    with HasNumericType[N, M]
    with HasInterpreter[N, M]
    with BasePropertiesTrait {

  override val logger: Logger = LoggerFactory.getLogger(getClass())
}
