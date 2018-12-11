package com.jakway.term.run.result

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.SimulationRun.{AllRunOutput, RunResultType}
import com.jakway.term.run.result.ChartProcessor.{OutputType, VariablePair}
import org.slf4j.{Logger, LoggerFactory}

/**
  *
  * @param excludeVariables
  * @param mustExcludeVariables if true, it's an error if a variable was included
  *                             in excludeVariables but could not be found
  * @param charts
  */
case class ChartConfig(excludeVariables: Set[String],
                       mustExcludeVariables: Boolean,
                       charts: Set[VariablePairChart])

case class VariablePairChart(title: String,
                             xVarName: String, yVarName: String,
                             altXAxisLabel: Option[String] = None,
                             altYAxisLabel: Option[String] = None) {
  val xAxisLabel = altXAxisLabel.getOrElse(xVarName)
  val yAxisLabel = altYAxisLabel.getOrElse(yVarName)

  def this(title: String,
           pair: VariablePair,
           altXAxisLabel: Option[String] = None,
           altYAxisLabel: Option[String] = None) {
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
}

object ChartProcessor {
  type OutputType = Either[SimError, Unit]

  class VariablePair(val inputVariable: String,
                     val outputVariable: String)

  class ChartProcessorError(override val msg: String)
    extends SimError(msg)

  case class ExcludeVariableError(override val msg: String)
    extends ChartProcessorError(msg)
}
