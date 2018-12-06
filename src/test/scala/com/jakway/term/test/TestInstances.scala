package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType
import com.jakway.term.numeric.types.implementations.DoublePrecision

package instances {

  object DoubleInst {
    type M = Double
    type N = NumericType[Double]

    val inst: N =
      NumericType.Implementations.getDoublePrecisionNumericTypeImplementation()
        .right.get
  }
  import com.jakway.term.test.framework.ToleranceInstances

  object BigDecimalInst {
    type M = java.math.BigDecimal
    type N = NumericType[M]
    val inst: N =
      NumericType.Implementations.getBigDecimalNumericTypeImplementation()
        .right.get
  }

  /**
    * TODO: not sure if these tests should extend NumericTypeTest, or if
    * NumericTypeTest is even necessary...
    */
  package double {
    import DoubleInst._

    class InstTestHasSubterms extends TestHasSubterms[N, M](inst)

    class InstTestFindVariables extends TestFindVariables[N, M](inst)

    class InstTestSubstituteFunctions
      extends TestSubstituteFunctions[N, M](inst)

    class InstTestInverseIdentitySimplifier
      extends TestInverseIdentitySimplifier[N, M](inst)

    class InstTestNullSubterms extends TestNullSubterms[N, M](inst)

    class InstTestSolver extends TestSolver[N, M](inst)
  }

  package bigdecimal {
    import BigDecimalInst._

    class InstTestHasSubterms extends TestHasSubterms[N, M](inst)

    class InstTestFindVariables extends TestFindVariables[N, M](inst)

    class InstTestSubstituteFunctions
      extends TestSubstituteFunctions[N, M](inst)

    class InstTestInverseIdentitySimplifier
      extends TestInverseIdentitySimplifier[N, M](inst)

    class InstTestNullSubterms extends TestNullSubterms[N, M](inst)

    class InstTestSolver extends TestSolver[N, M](inst)
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

    package double {
      import DoubleInst._
      class DoubleTrigTests extends TrigTests(inst, fiveDecimalPlaces.doubleEquality)
    }
  }
}
