package com.jakway.term.run.result.chart

import java.util.Comparator

import com.jakway.term.interpreter.InterpreterResult
import com.jakway.term.numeric.errors.SimError
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.XYSeries


/**
  *
  * @param excludeVariables
  * @param mustExcludeVariables if true, it's an error if a variable was included
  *                             in excludeVariables but could not be found
  * @param charts
  */
case class ChartConfig(comparator: Comparator[Any],
                       excludeVariables: Set[String],
                       charts: Set[VariablePairChart],
                       convertToNumber: InterpreterResult => Either[SimError, Number],
                       mustExcludeVariables: Boolean,
                       failOnIncompleteData: Boolean,
                       allowEmptyCharts: Boolean = false)

case class VariablePairChart(title: String,
                             xVarName: String, yVarName: String,
                             altXAxisLabel: Option[String] = None,
                             altYAxisLabel: Option[String] = None,
                             plotOrientation: PlotOrientation = PlotOrientation.VERTICAL,
                             legend: Boolean = true,
                             tooltips: Boolean = false,
                             urls: Boolean = false) {
  val xAxisLabel = altXAxisLabel.getOrElse(xVarName)
  val yAxisLabel = altYAxisLabel.getOrElse(yVarName)

  lazy val xySeries: XYSeries = new XYSeries(title, false, true)
}

object ChartConfig {
  sealed trait Output
  case object Display extends Output
  case class WriteToFile(where: String) extends Output
}

