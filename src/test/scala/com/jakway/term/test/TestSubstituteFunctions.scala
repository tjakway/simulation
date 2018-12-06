package com.jakway.term.test

import com.jakway.term
import com.jakway.term._
import com.jakway.term.elements._
import com.jakway.term.interpreter.warn.Warning
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.solver.SubstituteFunction
import com.jakway.term.solver.SubstituteFunction.Applications
import com.jakway.term.solver.SubstituteFunction.Applications.{Application, Inversion, Simplification}
import com.jakway.term.test.framework.TermMatchers
import org.scalatest.{FlatSpec, Matchers}

trait TestSubstituteFunctions[N <: NumericType[M], M]
  extends Matchers
    with TermMatchers
    with NumericTypeTest[N, M] { this: FlatSpec =>

  class SubstituteFunctionStep(val initial: Equation,
                               val inversion: Option[Inversion],
                               val simplification: Option[Simplification],
                               val result: Equation,
                               val warnings: Seq[Warning])

  class SubstituteFunctionTest(val solveFor: Variable[N, M],
                               val steps: Seq[SubstituteFunctionStep],
                               val initial: Equation,
                               val failWarnings: Boolean)

  case class SingleStepSubstituteFunctionTest(
                                    override val solveFor: Variable[N, M],
                                    override val initial: Equation,
                                    expectedResult: Equation)
    extends SubstituteFunctionTest(
      solveFor,
      //create the single test step
      Seq(new SubstituteFunctionStep(initial, None, None, expectedResult, Seq())),
      initial,
      true)

  class TestSubstituteFunction(val params: SubstituteFunctionTest)
    extends Matchers {
    import TermMatchers._

    def checkApplications(applications: Applications): Unit = {
      val expected = params.steps
      applications.applications.length shouldEqual expected.length

      applications.applications.zip(expected).foreach {
        case (thisApplication, thisExpectedStep) => {

          thisExpectedStep.inversion.foreach(checkInversion(thisApplication.inversion, _))
          //alternative test if no expected inversion is provided
          Util.ifNone(thisExpectedStep.inversion)(() => checkInversionNotEqual(thisApplication))

          thisExpectedStep.simplification.foreach(
            checkSimplification(thisApplication.simplification, _))
          checkWarnings(thisApplication.warnings, thisExpectedStep.warnings)

          thisApplication.result shouldEqual thisExpectedStep.result
        }
      }
    }

    def checkWarnings(actual: Seq[Warning], expected: Seq[Warning]): Unit = {
      if(params.failWarnings) {
        actual shouldEqual expected
      } else { }
    }

    def checkInversionNotEqual(application: Application): Unit = {
      application.inversion.left should not equal (application.simplification.left)
      application.inversion.right should equal (application.simplification.right)
    }

    def checkInversion(actual: Inversion, expected: Inversion): Unit = {
      assertMatchOrThrow(actual.left, expected.left)
      assertMatchOrThrow(actual.right, expected.left)
    }

    def checkSimplification(actual: Simplification, expected: Simplification): Unit = {
      assertMatchOrThrow(actual.left, expected.left)
      assertMatchOrThrow(actual.right, expected.left)
    }

    def doTest(): Unit = {
      val res = for {
        functions <- SubstituteFunction.mkSubstituteFunctions(
          params.solveFor, params.initial.left.asInstanceOf[HasSubterms])
        applications <- SubstituteFunction.applyFunctions(functions, params.initial)
      } yield {
        applications
      }
      res match {
        case Right(x) => checkApplications(x)
        case Left(e) => fail(s"Expected $res to be Right")
      }
    }

    def apply(): Unit = doTest()
  }

  "mkSubstituteFunctions" should "simplify x + y" in {
    val x = Variable[N, M]("x")
    val y = Variable[N, M]("y")
    val left = Add[N, M](x, y)
    val right: NumericTerm[N, M] = Literal("1")
    val eq = Equation(left, right)
    val test = SingleStepSubstituteFunctionTest(solveFor = x,
      initial = eq, expectedResult = Equation(x, Subtract(right, y)))

    new TestSubstituteFunction(test).doTest()
  }

  it should "simplify ln(x + y) = 5" in {
    val x = Variable[N, M]("x")
    val y = Variable[N, M]("y")
    val left = NaturalLogarithm(Add[N, M](x, y))
    val right: NumericTerm[N, M] = Literal("5")
    val initial = Equation(left, right)

    val firstStepEquation = Equation(Add[N, M](x, y),
      Power(Literal("e"), initial.right.asInstanceOf[NumericTerm[N, M]]))
    val expectedEquation: Equation = Equation(x,
      Subtract(Power(Literal("e"), right), y))

    val steps = Seq(
      new SubstituteFunctionStep(initial, None, None, firstStepEquation, Seq()),
      new SubstituteFunctionStep(firstStepEquation, None, None, expectedEquation, Seq()))

    val test = new SubstituteFunctionTest(x, steps, initial, true)
    new TestSubstituteFunction(test).doTest()
  }
}
