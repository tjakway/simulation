package com.jakway.term.run.result.chart

import java.io.File
import java.util.Comparator

import com.jakway.term.Util
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

sealed trait WriteChartsConfig {
  val encoding: String = "UTF-8"

  def getDest(inputVariable: String): Option[File]

  def checkConfig(): Either[SimError, WriteChartsConfig] = Right(this)
}

object WriteChartsConfig {
  case class ConfigError(override val msg: String)
    extends SimError(msg)
}

case class ToDirectory(dir: File) extends WriteChartsConfig {
  override def checkConfig(): Either[SimError, WriteChartsConfig] = {
    val checks: Seq[(String, File => Boolean)] = Seq(
      (s"$dir does not exist", (d: File) => d.exists()),
      (s"$dir is not a directory", (d: File) => d.isDirectory()),
      (s"Cannot write to $dir", (d: File) => d.canWrite())
    )
    Util.mapLeft(Util.foldChecks(checks, dir)(dir)) {
      msgs => WriteChartsConfig.ConfigError(s"Errors for $dir: " +
        com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(msgs))
    }.map(ToDirectory(_))
  }


  override def getDest(inputVariable: String): Option[File] = {
    Some(new File(dir, inputVariable))
  }
}

/**
  * @param files maps input variable -> filename
  */
case class ToFiles(files: Map[String, File]) extends WriteChartsConfig {
  override def getDest(inputVariable: String): Option[File] =
    files.get(inputVariable)
}
