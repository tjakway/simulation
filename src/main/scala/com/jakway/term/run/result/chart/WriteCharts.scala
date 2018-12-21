package com.jakway.term.run.result.chart

import java.io.File

import com.jakway.term.Util
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.result.chart.WriteCharts.OutputFileErrors
import org.jfree.chart.JFreeChart

import scala.concurrent.{ExecutionContext, Future}

class WriteCharts(val config: WriteChartsConfig)
  extends ChartProcessor.ResultProcessor[WriteCharts.OutputType] {

  override def apply(results: ChartProcessor.OutputType)
                    (implicit ec: ExecutionContext): Future[WriteCharts.OutputType] = {
    ???
  }

  private def getDests(in: Map[VariablePairChart, JFreeChart]): Map[File, JFreeChart] = {
    ???
  }
  private def checkAllOutputFiles(files: Set[File]): Either[SimError, Set[File]]= {
    val allErrors =
      files.foldLeft(Map(): Map[File, Seq[String]]) {
        case (accErrors, thisFile) => {
          Util.foldChecks(fileChecks, files)(thisFile) match {
            case Left(errs) => accErrors.updated(thisFile, errs)
            case Right(_) => accErrors
          }
        }
      }

    if(allErrors.isEmpty) {
      Right(files)
    } else {
      val collectedErrs = allErrors.foldLeft(Seq()) {
        case (acc, (thisFile, errs)) => {
          val msg = s"Errors for $thisFile: $errs"
          acc :+ msg
        }
      }

      val msg = "Caught errors in output files: " +
        com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(collectedErrs)
      Left(OutputFileErrors(msg))
    }
  }

  private def addExtension(f: File): File = {
    new File(f.getPath + ".svg")
  }

  private val fileChecks: Seq[(String, File => Boolean)] = {
    Seq(
      ("Cannot write", _.canWrite()),
      ("Already exists", !_.exists())
    )
  }
}

object WriteCharts {
  type OutputType = Either[SimError, Seq[File]]

  case class OutputFileErrors(override val msg: String)
    extends SimError(msg)
}
