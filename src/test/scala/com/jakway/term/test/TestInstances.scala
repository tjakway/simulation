package com.jakway.term.test

import com.jakway.term.numeric.types.NumericType

package instances {

  object DoubleInst {
    type M = Double
    type N = NumericType[Double]

    val inst: N =
      NumericType.Implementations.getDoublePrecisionNumericTypeImplementation()
        .right.get
  }
  import com.jakway.term.test.eval.TrigTests
  import com.jakway.term.test.framework.ToleranceInstances
  import com.jakway.term.test.gen.TestGenTerm
  import org.scalatest.FlatSpec

  object BigDecimalInst {
    type M = java.math.BigDecimal
    type N = NumericType[M]
    val inst: N =
      NumericType.Implementations.getBigDecimalNumericTypeImplementation()
        .right.get
  }

  class AllTestInstances[N <: NumericType[M], M](override val numericType: N)
    extends FlatSpec
      with TestHasSubterms[N, M]
      with TestFindVariables[N, M]
      with TestSubstituteFunctions[N, M]
      with TestInverseIdentitySimplifier[N, M]
      with TestNullSubterms[N, M]
      with TestGenTerm[N, M]
      with TestNumericTypeCmp[N, M]

  abstract class AllInterpreterTestInstances[N <: NumericType[M], M]
    (override val numericType: N, val toleranceInstances: ToleranceInstances)
      extends FlatSpec
        with TrigTests[N, M]

  object Tolerances {
    lazy val exact: ToleranceInstances =
      ToleranceInstances.getOrThrow("0")

    lazy val doubleMinimum: ToleranceInstances =
      ToleranceInstances.getOrThrow(Double.MinValue.abs.toString)

    lazy val fiveDecimalPlaces: ToleranceInstances =
      ToleranceInstances.getToDecimalPlaces(5).right.get
  }

  /**
    * TODO: not sure if these tests should extend NumericTypeTest, or if
    * NumericTypeTest is even necessary...
    */
  package double {
    import com.jakway.term.test.gen.EvalProperties
    import com.jakway.term.test.instances.DoubleInst._
    import com.jakway.term.test.run.{SimulationRunProperties, TestSimulationRun}
    import org.scalacheck.Properties
    import org.scalactic.Equality

    class DoubleAllTestInstances extends AllTestInstances[N, M](inst)
    class DoubleInterpreterTestInstances
      extends AllInterpreterTestInstances[N, M](inst, Tolerances.fiveDecimalPlaces) {
      override implicit val equality: Equality[M] = toleranceInstances.doubleEquality
    }

    class DoubleEvalProperties
      extends Properties("DoubleEvalProperties")
      with EvalProperties[N, M] {
      override val numericType: N = inst

      override protected def preventOverflow: Boolean = false
    }

    class DoubleNumericTypeCmpProperties
      extends Properties("DoubleNumericTypeCmpProperties")
      with NumericTypeCmpProperties[N, M] {
      override val numericType: N = inst
    }
  }

  package bigdecimal {
    import com.jakway.term.test.gen.EvalProperties
    import com.jakway.term.test.instances.BigDecimalInst._
    import org.scalacheck.Properties
    import org.scalactic.Equality

    class BigDecimalAllTestInstances extends AllTestInstances[N, M](inst)

    class BigDecimalInterpreterTestInstances
      extends AllInterpreterTestInstances[N, M](inst, ToleranceInstances.getToDecimalPlaces(30).right.get) {
      override implicit val equality: Equality[M] = toleranceInstances.bigDecimalEquality
    }

    class BigDecimalEvalProperties
      extends Properties("BigDecimalEvalProperties")
        with EvalProperties[N, M] {
      override val numericType: N = inst

      override protected def preventOverflow: Boolean = true
    }


    class BigDecimalNumericTypeCmpProperties
      extends Properties("BigDecimalNumericTypeCmpProperties")
        with NumericTypeCmpProperties[N, M] {
      override val numericType: N = inst
    }
  }
}
