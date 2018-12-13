package com.jakway.term.run.result.chart

import com.jakway.term.interpreter.InterpreterResult
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.AllRunOutput
import com.jakway.term.run.result.chart.ChartProcessor._
import org.jfree.data.xy.XYSeries
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}


class VariableDataProcessor(
  val convertToNumber: InterpreterResult => Either[SimError, Number],
  val failOnIncompleteData: Boolean) {
  import VariableDataProcessor._

  val logger: Logger = LoggerFactory.getLogger(getClass())

  private def getVariableDataAndErrors(runOutput: AllRunOutput)
                                      (variablePair: VariablePair):
  (Set[SimError], Seq[VariablePairValues[Data, Data]]) = {

    val empty:(Set[SimError], Seq[VariablePairValues[Data, Data]]) = (Set(), Seq())

    runOutput.runs.foldLeft(empty) {
      case ((errs, acc), thisRunOutput) => {
        thisRunOutput.input.get(variablePair.inputVariable) match {
          case Some(foundInputValue) => {

            lazy val error = InputVariableNotInterpreterResultError(
              variablePair.inputVariable, foundInputValue
            )

            Try(foundInputValue.asInstanceOf[InterpreterResult]).map {
              castInputValue =>
                val newDataPair =
                  new VariablePairValues[Data, Data](
                    castInputValue,
                    thisRunOutput.runOutput)

                (errs, acc :+ newDataPair)
            }
              .getOrElse(errs + error, acc)
          }
          case None => {
            val errMsg = s"Could not find input variable " +
              s"${variablePair.inputVariable} in ${thisRunOutput.input}"
            (errs + VariableNotFoundError(errMsg), acc)
          }

        }
      }

    }
  }

  def getVariableData(runOutput: AllRunOutput)(variablePair: VariablePair):
  Either[SimError, Seq[VariablePairValues[Data, Data]]] = {
    val (errs, res) = getVariableDataAndErrors(runOutput)(variablePair)

    //partition errors so we can handle the ones we may have to ignore
    val (incompleteDataErrors, otherErrors) = {
      val empty: (Set[SimError], Set[SimError]) = (Set(), Set())
      errs.foldLeft(empty) {
        case ((as, bs), thisErr) => {
          thisErr match {
            case t: VariableNotFoundError => (as + t, bs)
            case _ => (as, bs + thisErr)
          }
        }
      }
    }


    def returnOrFail(errors: Set[SimError]): Either[SimError, Seq[VariablePairValues[Data, Data]]] =
      if(errors.isEmpty) {
        Right(res)
      } else {
        Left(VariableDataErrors(errors))
      }

    //note that these are ***potentially*** ignorable errors
    val ignorableErrors = incompleteDataErrors

    val unignorableErrors =
      if(failOnIncompleteData) {
        ignorableErrors ++ otherErrors
      } else {
        //warn about the ignorable errors anyway
        ignorableErrors.foreach(e => logger.warn(e.toString))
        otherErrors
      }

    returnOrFail(unignorableErrors)
  }

  def processVariableData(data: Seq[VariablePairValues[Data, Data]],
                          series: XYSeries): Either[SimError, Unit] = {
    val empty: Either[SimError, Unit] = Right({})

    data.foldLeft(empty) {
      case (Right(_), thisPair) => {
        val r  = Try {
          for {
            x <- convertToNumber(thisPair.inputData)
            y <- convertToNumber(thisPair.outputData)
          } yield {
            series.add(x, y)
          }
        }
        r match {
          case Success(x) => x
          case Failure(t) => {
            val e = new VariableDataError(s"Caught exception: $t")
            e.initCause(t)
            Left(e)
          }
        }
      }

    case (Left(err), _) => Left(err)
    }
  }
}


object VariableDataProcessor {
  type Data = InterpreterResult
}
