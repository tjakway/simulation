package com.jakway.term.test.framework

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.NumericTypeTest
import com.jakway.term.test.framework.InterpreterTest.ReadLiteralError
import org.scalactic.Equality

class InterpreterTest[N <: NumericType[M], M]
  (val numericType: N, implicit val equality: Equality[M])
  extends NumericTypeTest[N, M] {

  //for convenience
  def readLiteral(x: String): M = {
    numericType.readLiteral(x) match {
      case Right(y) => y
      case Left(err) =>
        throw ReadLiteralError(s"Error in InterpreterTest.readLiteral: $err")
    }
  }
}

object InterpreterTest {
  case class ReadLiteralError(override val msg: String)
    extends TestError(msg)
}
