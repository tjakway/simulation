package com.jakway.term.run.result.chart

import com.jakway.term.Util
import com.jakway.term.elements.{Literal, Term}
import com.jakway.term.interpreter.{InterpreterResult, Raw}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.{AllRunOutput, RunResultType}
import com.jakway.term.run.result.ResultProcessor
import com.jakway.term.run.result.chart.ChartProcessor.{OutputType, VariablePair}
import com.jakway.term.run.result.chart.VariableDataProcessor.Data
import org.jfree.data.xy.{XYDataset, XYSeries}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

/**
  *
  * @param excludeVariables
  * @param mustExcludeVariables if true, it's an error if a variable was included
  *                             in excludeVariables but could not be found
  * @param charts
  */
case class ChartConfig(excludeVariables: Set[String],
                       charts: Set[VariablePairChart],
                       convertToNumber: InterpreterResult => Either[SimError, Number],
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

  val variableDataProcessor: VariableDataProcessor =
    new VariableDataProcessor(chartConfig.convertToNumber,
      chartConfig.failOnIncompleteData)

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


  private def x(runOutput: AllRunOutput): Unit = {
    val pairs = getVariablePairs(runOutput)

    type PairDataType = Either[Seq[SimError],
      Map[VariablePair, Seq[VariablePairValues[Data, Data]]]]
    val empty: PairDataType = Right(Map())

    val pairData: PairDataType =
      pairs.map(thisPair =>
            variableDataProcessor.
              getVariableData(runOutput)(thisPair)
      .map(data => (thisPair, data)))
      .foldLeft(empty) {
        case (l, Left(err)) => Util.appendLeftOrReplace(l, err)
        case (Left(es), Right(x)) => Left(es)
        case (Right(acc), Right(next)) =>
          Right(acc.updated(next._1, next._2))
      }


    def associateCharts(pairs: Map[VariablePair, Seq[VariablePairValues[Data, Data]]]):
      Either[Seq[SimError], Map[VariablePairChart, Seq[VariablePairValues[Data, Data]]]] = {

      val newPairs: Seq[Either[SimError,
        (VariablePairChart, Seq[VariablePairValues[Data, Data]])]] =
        pairs.map {
        case (key, values) =>
          findChartForVariable(key).map(newKey => (newKey, values))
      }.toSeq

      Util.accEithers(newPairs).map(_.toMap)
    }

    for {
      pairs <- pairData
      chartsWithData <- associateCharts(pairs)

      //next: create XYSeries for each chart & fill with data
    }
  }

  private def findChartForVariable(variable: VariablePair):
    Either[SimError, VariablePairChart] = {
    chartConfig.charts.find(_.xVarName == variable.inputVariable)
      .map(Right(_))
      .getOrElse(Left(VariableNotInCharts(variable.inputVariable,
        chartConfig.charts)))
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
    extends VariableDataError(msg)

  case class InputVariableNotInterpreterResultError(varName: String,
                                                    varValue: Term)
    extends VariableDataError(s"Could not graph $varName because" +
      s" it has a value of ${varValue} which is not an instance of" +
      s" InterpreterResult")

  class VariableDataError(override val msg: String)
    extends ChartProcessorError(msg)

  case class VariableDataErrors(errs: Set[SimError])
    extends VariableDataError(
      s"Errors while processing variable data: " +
      com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(errs.toSeq))

  case class VariableNotInCharts(varName: String, charts: Set[VariablePairChart])
    extends VariableDataError(s"Could not find $varName in the provided" +
      s" charts: " + charts.toString + " (did you forget to exclude it?)")

  /**
    * too many charts, not enough variables
    * @param variables
    * @param charts
    */
  case class VariablesNotFoundForCharts(variables: Seq[VariablePair],
                                        charts: Set[VariablePairChart])
    extends VariableDataError(s"Variables were not found for these charts: " +
      s"${charts.toString} (variables: ${variables.map(_.inputVariable)})")
}
