package com.jakway.rocket

import com.jakway.term.elements._
import com.jakway.term.numeric.types.NumericType

class IdealRocketEquation[N <: NumericType[M], M] {

  val mf = Variable[N, M]("mf", "full rocket mass")
  val me = Variable[N, M]("me", "empty rocket mass")

  val isp = Variable[N, M]("isp", "specific impulse")
  val gravity = Variable[N, M]("g0", "gravitational constant")

  //see https://spaceflightsystems.grc.nasa.gov/education/rocket/rktpow.html
  val propellantMassRatio: TermBodyFunction[N, M] = {
    new TermBodyFunction[N, M](Seq(mf, me)) {
      override def body: Term = Divide(mf, me)
    }
  }

  //veq
  val equivalentExitVelocity: TermBodyFunction[N, M] = {
    new TermBodyFunction[N, M](Seq(isp, gravity)) {
      override def body: Term = Multiply(isp, gravity)
    }
  }

  //rocket velocity
  val deltaU: TermBodyFunction[N, M] = {

    new TermBodyFunction[N, M](Seq(mf, me, isp, gravity)) {
      override def body: Term =
        Multiply(equivalentExitVelocity(isp, gravity),
          NaturalLogarithm(propellantMassRatio(mf, me)))
    }
  }

}
