package com.jakway.term.test.gen

import java.util.Locale

import com.jakway.term.interpreter.Interpreter
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.run.SimulationRun
import com.jakway.term.test.framework.gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.concurrent.ExecutionContext

trait SimulationRunProperties[N <: NumericType[M], M]
  extends HasInterpreter[N, M]
    with GenSimulationRun[N, M]
    with GenEval[N, M]
    with BasePropertiesTrait {
  this: Properties =>
  import SimulationRunProperties._

  //******************************************************
  //config values
  //******************************************************
  val maxNumConstantVariables: Option[Int] = None
  val maxNumDynamicVariables: Option[Int] = Some(4)
  //******************************************************

  private def getGenSimulationRun(): Gen[SimulationRun] = {
    val res = genSimulationRun(false,
      maxNumConstantVariables, maxNumDynamicVariables) match {
      case Right(x) => x
      case Left(e) => throw GenSimulationRunError(e)
    }
    res.filter(_ != null)
  }

  private implicit val arbSimulationRun: Arbitrary[SimulationRun] =
    Arbitrary(getGenSimulationRun())

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  property("execute SimulationRun without error") =
    forAll { (simulationRun: SimulationRun) =>
      assert(simulationRun != null)
      checkNumericType[N, M](numericType)
      checkInterpreter(interpreter)

      checkRun(simulationRun, interpreter)
  }

  /**
    * throws AbstractMethodError if checkRun is private...
    * @param run
    * @param interpreter
    * @return
    */
  def checkRun(run: SimulationRun, interpreter: Interpreter): Boolean = {
    val res = SimulationRun.run(run, interpreter)
    //warn on error
    res match {
      case Left(e) =>  {
        val fmt = new java.util.Formatter(new StringBuffer(), Locale.getDefault())
        fmt.format(s"Scalacheck run failed for SimulationRun=$run:\n")
        fmt.format(s"\tError: $e")
        logger.warn(fmt.toString)
      }
      case _ => {}
    }

    res.isRight
  }

}

object SimulationRunProperties {
  class SimulationRunPropertiesError(override val msg: String)
    extends GenError(msg) {
    def this(t: Throwable) {
      this(t.toString)
    }
  }

  case class GenSimulationRunError(e: SimError)
    extends SimulationRunPropertiesError(s"Error in genSimulationRun: $e")

  case object NullNumericTypeError
    extends SimulationRunPropertiesError(
      "numericType was null at beginning of property execution")

  case object NullInterpreterError
    extends SimulationRunPropertiesError(
      "interpreter was null at beginning of property execution")

  def checkNumericType[N <: NumericType[M], M](n: N): Unit =
    if(n == null) {
      throw NullNumericTypeError
    } else {}

  def checkInterpreter(interpreter: Interpreter): Unit = {
    if(interpreter == null) {
      throw NullInterpreterError
    } else {}
  }
}
