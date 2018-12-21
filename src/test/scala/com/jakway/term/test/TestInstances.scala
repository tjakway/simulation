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
    import com.jakway.term.test.framework.gen.BaseProperties
    import com.jakway.term.test.gen.{EvalProperties, SimulationRunProperties}
    import com.jakway.term.test.instances.DoubleInst._
    import org.scalacheck.Properties
    import org.scalactic.Equality

    class DoubleAllTestInstances extends AllTestInstances[N, M](inst)
    class DoubleInterpreterTestInstances
      extends AllInterpreterTestInstances[N, M](inst, Tolerances.fiveDecimalPlaces) {
      override implicit val equality: Equality[M] = toleranceInstances.doubleEquality
    }

    class DoubleEvalProperties
      extends BaseProperties[N, M]("DoubleEvalProperties", inst)
      with EvalProperties[N, M] {
      override protected def preventOverflow: Boolean = false
    }

    class DoubleNumericTypeCmpProperties
      extends BaseProperties[N, M]("DoubleNumericTypeCmpProperties", inst)
      with NumericTypeCmpProperties[N, M]

    class DoubleSimulationRunProperties
      extends BaseProperties[N, M]("DoubleSimulationRunProperties", inst)
      with SimulationRunProperties[N, M]
  }

  package bigdecimal {
    import com.jakway.term.test.framework.gen.BaseProperties
    import com.jakway.term.test.gen.{EvalProperties, SimulationRunProperties}
    import com.jakway.term.test.instances.BigDecimalInst._
    import org.scalacheck.Properties
    import org.scalactic.Equality

    class BigDecimalAllTestInstances extends AllTestInstances[N, M](inst)

    class BigDecimalInterpreterTestInstances
      extends AllInterpreterTestInstances[N, M](inst, ToleranceInstances.getToDecimalPlaces(30).right.get) {
      override implicit val equality: Equality[M] = toleranceInstances.bigDecimalEquality
    }

    class BigDecimalEvalProperties
      extends BaseProperties[N, M]("BigDecimalEvalProperties", inst)
        with EvalProperties[N, M] {
      override protected def preventOverflow: Boolean = true
    }


    class BigDecimalNumericTypeCmpProperties
      extends BaseProperties[N, M]("BigDecimalNumericTypeCmpProperties", inst)
        with NumericTypeCmpProperties[N, M]


    class BigDecimalSimulationRunProperties
      extends BaseProperties[N, M]("BigDecimalSimulationRunProperties", inst)
        with SimulationRunProperties[N, M]
  }
}
