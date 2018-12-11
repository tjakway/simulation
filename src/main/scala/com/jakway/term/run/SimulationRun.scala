package com.jakway.term.run

import java.util.{Formatter, Locale}

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.interpreter.{Interpreter, InterpreterResult}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.Errors.{ExpectedInterpreterResultError, RunFailed, SimulationRunError}
import com.jakway.term.run.SimulationRun.ValueStreams
import com.jakway.term.solver.Solvable

import scala.concurrent.{ExecutionContext, Future}

object SimulationRun {
  type ValueStreams = Map[String, Stream[Term]]

  class SingleRunOutput(val input: SymbolTable,
                        val runOutput: InterpreterResult)

  class AllRunOutput(val runs: Traversable[SingleRunOutput],
                     val outputVariable: String,
                     val toRun: Solvable,
                     val constants: SymbolTable)

  type RunResultType = Future[Either[SimError, AllRunOutput]]

  private type RawRunResultType =
    Future[Either[Seq[(SymbolTable, SimError)],
                      Seq[SingleRunOutput]]]

  type RunResultFormatter =
    ExecutionContext =>
      String =>
      Solvable =>
      SymbolTable =>
      RawRunResultType =>
      RunResultType

  def formatRunResults: RunResultFormatter = {
    (ec: ExecutionContext) =>
      (outputVariable: String) =>
        (toRun: Solvable) =>
          (constants: SymbolTable) =>
          (result: RawRunResultType) => {

            result.map { r =>
              r match {
                case Left(errors) => {
                  val fmt: Formatter = new java.util.Formatter(new StringBuffer(),
                    Locale.getDefault())

                  fmt.format("Simulation Run Errors\n")
                  fmt.format("Total: " + errors.length + "\n")
                  errors.foreach {
                    case (input, err) => fmt.format(s"\tInput $input -> $err")
                  }
                  Left(RunFailed(fmt.toString))
                }
                case Right(outputs) => Right(
                  new AllRunOutput(outputs, outputVariable, toRun, constants))
              }
            }(ec)
          }
  }

  object ErrorBehavior {
    type DesiredResult = SingleRunOutput
    type NextType = Either[(SymbolTable, SimError), DesiredResult]
    type AccumulatorType = Either[Seq[(SymbolTable, SimError)],
                            Seq[DesiredResult]]
  }
  import ErrorBehavior._

  trait ErrorBehavior {
    def reduce(next: NextType, acc: AccumulatorType): AccumulatorType
  }

  /**
    * Accumulate errors if any are found
    */
  object IfErrorNoResult extends ErrorBehavior {
    override def reduce(
        next: NextType,
        acc: AccumulatorType): AccumulatorType = {

      next match {
        case Left(err) => {
          acc match {
            case Left(errs) => Left(errs :+ err)
            case Right(_) => Left(Seq(err))
          }
        }
        case Right(x) => acc match {
            //ignore success if we've already found an error
          case Left(errs) => Left(errs)
            //otherwise accumulate
          case Right(xs) => Right(xs :+ x)
        }
      }
    }
  }

  object IgnoreErrors extends ErrorBehavior {
    override def reduce(
           next: NextType,
           acc: AccumulatorType): AccumulatorType = {

      next match {
        case Left(err) => acc
        case Right(x) => acc match {
          case Right(xs) => Right(xs :+ x)
          case Left(_) => Right(Seq())
        }
      }
    }
  }

  object Errors {
    class SimulationRunError(override val msg: String)
      extends SimError(msg)

    case class ExpectedInterpreterResultError(t: Term)
      extends SimulationRunError(s"Expected Interpreter.eval to yield" +
        s" an instance of InterpreterResult but got $t instead")

    case class RunFailed(override val msg: String)
      extends SimError(msg)
  }

  def filterForInterpreterResult(t: Term): Either[(SymbolTable, SimError), InterpreterResult] =
    t match {
      case x: InterpreterResult => Right(x)
      case _ => Left((Map(), ExpectedInterpreterResultError(t)))
    }

  def run(runData: SimulationRun,
          interpreter: Interpreter,
          formatter: RunResultFormatter,
          errorBehavior: ErrorBehavior = IfErrorNoResult)
         (implicit executor: ExecutionContext): Either[SimError, RunResultType] = {

    runData.computeValues.getSymbolTables(runData.inputs).map {
      symbolTables =>

        val toRun = runData.toRun.otherSide
        val allFutures = symbolTables.map { thisTable =>
          Future {
            interpreter.eval(thisTable)(toRun) match {
              case Right(success) => Right((thisTable, success))
              //combine the symbol table with the error so we can keep track
              //of which values caused which errors
              case Left(err) => Left((thisTable, err))
            }
          }
        }

        val empty: Future[AccumulatorType] = Future(Right(Seq()))
        val foldResult = allFutures.foldLeft(empty) {
          case (fAcc, f) => {
            for {
              acc <- fAcc
              term <- f
            } yield {
              val res: Either[(SymbolTable, SimError), SingleRunOutput] =
                term.flatMap {
                  case (input, output) =>
                    filterForInterpreterResult(output)
                      .map(new SingleRunOutput(input, _))
                }
              errorBehavior.reduce(res, acc)
            }
          }
        }

        formatter(executor)(runData.outputVariable)(runData.toRun)(runData.computeValues.constants)(foldResult)
    }
  }
}

class SimulationRun(val inputs: ValueStreams,
                    val outputVariable: String,
                    val toRun: Solvable,
                    val computeValues: ComputeValues = new Combinations())
