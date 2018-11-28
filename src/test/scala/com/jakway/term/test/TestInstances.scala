package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.numeric.types.implementations.DoublePrecision

package instances {

  object DoubleInst {
    type M = Double
    type N = NumericType[Double]

    val inst =
      NumericType.Implementations.getDoublePrecisionNumericTypeImplementation()
        .right.get
  }
  import com.jakway.term.test.framework.ToleranceInstances

  /**
    * TODO: not sure if these tests should extend NumericTypeTest, or if
    * NumericTypeTest is even necessary...
    */
  package double {


    import DoubleInst._

    class DoubleTestHasSubterms extends TestHasSubterms[N, M](inst)

    class DoubleTestFindVariables extends TestFindVariables[N, M](inst)

    class DoubleTestSubstituteFunctions
      extends TestSubstituteFunctions[N, M](inst)

    class DoubleTestInverseIdentitySimplifier
      extends TestInverseIdentitySimplifier[N, M](inst)

    class DoubleTestNullSubterms extends TestNullSubterms[N, M](inst)

    class DoubleTestSolver extends TestSolver[N, M](inst)
  }

  object Tolerances {
    lazy val exact: ToleranceInstances =
      ToleranceInstances.getOrThrow("0")

    lazy val doubleMinimum: ToleranceInstances =
      ToleranceInstances.getOrThrow(Double.MinValue.abs.toString)

    lazy val fiveDecimalPlaces: ToleranceInstances =
      ToleranceInstances.getToDecimalPlaces(5).right.get
  }

  package interpreter {
    import Tolerances._
    import com.jakway.term.test.eval.TrigTests

    import DoubleInst._
    class DoubleTrigTests extends TrigTests(inst, fiveDecimalPlaces.doubleEquality)
  }
}
