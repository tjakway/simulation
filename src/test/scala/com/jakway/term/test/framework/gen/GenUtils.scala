package com.jakway.term.test.framework.gen

import org.scalacheck.Gen

object GenUtils {
  /**
    * ScalaCheck really needs an equivalent to Haskell's "return" function
    * to lift values into the type
    * @param x
    * @tparam A
    * @return
    */
  def wrap[A](x: A): Gen[A] = {
    Gen.alphaNumStr.map(_ => x)
  }
}
