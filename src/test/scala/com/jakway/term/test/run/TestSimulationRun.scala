package com.jakway.term.test.run

import com.jakway.term.elements.NumericTerm
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.GenTerm
import org.scalacheck.{Arbitrary, Properties}
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Ignore

trait BasicSimulationRunTests { this: FlatSpec =>

  "SimulationRun" should "return the same value for id" in {

  }
}

class TestSimulationRun {

}


trait SimulationRunProperties[N <: NumericType[M], M]
   { this: Properties =>

  val numericType: N

  val genTerm: GenTerm[N, M] = new GenTerm[N, M](numericType)

  implicit val arbNumericTerm: Arbitrary[NumericTerm[N, M]] =
    Arbitrary(genTerm.genNumericTerm)
}
