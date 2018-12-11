package com.jakway.term.run.result

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.InterpreterResult
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.{AllRunOutput, RunResultType}
import com.jakway.term.run.result.ChartProcessor.{OutputType, VariablePair}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

/**
  *
  * @param excludeVariables
  * @param mustExcludeVariables if true, it's an error if a variable was included
  *                             in excludeVariables but could not be found
  * @param charts
  */
case class ChartConfig(excludeVariables: Set[String],
                       charts: Set[VariablePairChart],
                       mustExcludeVariables: Boolean,
                       failOnIncompleteData: Boolean)

case class VariablePairChart(title: String,
                             xVarName: String, yVarName: String,
                             altXAxisLabel: Option[String] = None,
                             altYAxisLabel: Option[String] = None) {
  val xAxisLabel = altXAxisLabel.getOrElse(xVarName)
  val yAxisLabel = altYAxisLabel.getOrElse(yVarName)

  def this(title: String,
           pair: VariablePair,
           altXAxisLabel: Option[String],
           altYAxisLabel: Option[String]) {
    this(title, pair.inputVariable, pair.outputVariable,
      altXAxisLabel, altYAxisLabel)
  }
}

object ChartConfig {
  sealed trait Output
  case object Display extends Output
  case class WriteToFile(where: String) extends Output
}

class ChartProcessor(val chartConfig: ChartConfig)
  extends ResultProcessor[OutputType] {
  import ChartProcessor._

  val logger: Logger = LoggerFactory.getLogger(getClass())

  override def apply(results: RunResultType): OutputType = {

    ???
  }

  def excludeVariables(pairs: Set[VariablePair]): Either[SimError, Set[VariablePair]] = {
    val excludeVariables = chartConfig.excludeVariables

    val empty: (Set[String], Set[String], Set[VariablePair]) = (Set(), Set(), Set())

    val (errors, excludedVars, accPairs) = pairs.foldLeft(empty) {
      case ((errors, excludedVars, accPairs), thisPair) => {
        val nextErrors =
          if(excludeVariables.contains(thisPair.outputVariable)) {
            errors + s"Cannot exclude ${thisPair.outputVariable}: it is the output variable"
          } else {
            errors
          }


        val (nextExcludedVars, nextAccPairs) =
          if(excludeVariables.contains(thisPair.inputVariable)) {
            (excludeVariables + thisPair.inputVariable, accPairs)
          } else {
            (excludeVariables, accPairs + thisPair)
          }

        (nextErrors, nextExcludedVars, nextAccPairs)
      }
    }

    //handle any variables we should have excluded but didn't find
    val notFoundVars = excludeVariables.diff(excludedVars)
    val notFoundError: Option[String] =
      if(notFoundVars.isEmpty) {
        None
      } else {
        val varsStr: String = {
          val foldRes = notFoundVars.foldLeft("") {
            case (acc, thisVar) => acc + ", " + thisVar.toString
          }
          "{ " + foldRes  + " }"
        }
        val errMsg = "Asked to exclude these variables but could not find them: " + varsStr

        //if it's not an error, warn and continue
        if(chartConfig.mustExcludeVariables) {
          Some(errMsg)
        } else {
          logger.warn(errMsg)
          None
        }
      }


    val e = notFoundError match {
      case Some(x) => errors + x
      case None => errors
    }

    if(e.isEmpty) {
      Right(accPairs)
    } else {
      Left(ExcludeVariableError(
        com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(e.toSeq)))
    }
  }

  def getVariablePairs(runOutput: AllRunOutput): Set[VariablePair] = {
    runOutput.runs.headOption match {
      case Some(firstRun) =>
        //graph non-constants
        firstRun.input.keySet.diff(runOutput.constants.keySet)
          .map(thisInputVariable =>
            new VariablePair(thisInputVariable, runOutput.outputVariable))

      case None => Set()
    }
  }

  class VariableData(val failOnIncompleteData: Boolean) {
    type Data = InterpreterResult

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
  }
}

object ChartProcessor {
  type OutputType = Either[SimError, Unit]

  class VariablePair(val inputVariable: String,
                     val outputVariable: String)

  class VariablePairValues[A, B](val inputData: A, val outputData: B)

  class ChartProcessorError(override val msg: String)
    extends SimError(msg)

  case class ExcludeVariableError(override val msg: String)
    extends ChartProcessorError(msg)

  case class VariableNotFoundError(override val msg: String)
    extends ChartProcessorError(msg)

  case class InputVariableNotInterpreterResultError(varName: String,
                                                    varValue: Term)
    extends ChartProcessorError(s"Could not graph $varName because" +
      s" it has a value of ${varValue} which is not an instance of" +
      s" InterpreterResult")

  case class VariableDataErrors(errs: Set[SimError])
    extends ChartProcessorError(
      s"Errors while processing variable data: " +
      com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(errs.toSeq))
}
