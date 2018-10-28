package com.jakway.term.test

import com.jakway.term._
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.SubstituteFunction
import com.jakway.term.solver.SubstituteFunction.Applications
import org.scalatest.{FlatSpec, Matchers}

abstract class TestSubstituteFunctions[N <: NumericType[M], M]
  (override val numericType: N)
  extends FlatSpec with Matchers with NumericTypeTest[N, M] {

  "mkSubstituteFunctions" should "simplify x + y" in {
    val x = Variable[N, M]("x")
    val y = Variable[N, M]("y")
    val left = Add[N, M](x, y)
    val right: NumericTerm[N, M] = Literal("1")
    val eq = Equation(left, right)


    val res = for {
      functions <- SubstituteFunction.mkSubstituteFunctions(x, left, left.identity)
      application <- SubstituteFunction.applyFunctions(functions, eq)
    } yield {
      application
    }

    val expectedEquation = Equation(y, Subtract(right, y))
    def checkApplications(a: Applications): Unit = {
      a.applications.length shouldEqual 1
      val application = a.applications.head
      application.inversion.left should not equal (application.simplification.left)
      application.inversion.right should not equal (application.simplification.right)


      a.start shouldEqual eq
      a.result shouldEqual expectedEquation
    }

    res match {
      case Right(x) => checkApplications(x)
      case Left(e) => throw e
    }
  }
}
