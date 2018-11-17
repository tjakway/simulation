package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.numeric.types.implementations.DoublePrecision

package instances {

  object Double {
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


    import Double._

    class DoubleTestHasSubterms extends TestHasSubterms[N, M](inst)

    class DoubleTestFindVariables extends TestFindVariables[N, M](inst)

    class DoubleTestSubstituteFunctions
      extends TestSubstituteFunctions[N, M](inst)

    class DoubleTestInverseIdentitySimplifier
      extends TestInverseIdentitySimplifier[N, M](inst)

    class DoubleTestNullSubterms extends TestNullSubterms[N, M](inst)
  }

  object ExactTests {
    lazy val exact: ToleranceInstances =
      ToleranceInstances.getOrThrow("0")
  }

  package interpreter {
    import ExactTests._
    import com.jakway.term.test.eval.TrigTests

    import Double._
    class DoubleTrigTests extends TrigTests(inst, exact.doubleEquality)
  }
}
