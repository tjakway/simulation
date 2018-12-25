package com.jakway.term.run.result.chart

import java.io.File

import com.jakway.term.Util
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.result.chart.WriteCharts.{OutputFileErrors, OutputFileNotFound}
import org.jfree.chart.JFreeChart

import scala.concurrent.{ExecutionContext, Future}

class WriteCharts(val config: WriteChartsConfig)
  extends ChartProcessor.ResultProcessor[WriteCharts.OutputType] {

  val outputExtension = ".svg"

  override def apply(results: ChartProcessor.OutputType)
                    (implicit ec: ExecutionContext): Future[Either[SimError, WriteCharts.OutputType]] = {
    Future {
      results.map { r =>
        for {
          dests <- getDests(r)
          _ <- checkAllOutputFiles(dests.keySet)
          out <- writeCharts(dests)
        } yield out
      }
    }
  }

  private def getDests(in: Map[VariablePairChart, JFreeChart]):
    Either[SimError, Map[File, JFreeChart]] = {

    def getOutputFile(in: (VariablePairChart, JFreeChart)):
      Either[SimError, (File, JFreeChart)] = {

      val (pair, chart) = in
      config.getDest(pair.xVarName) match {
        case Some(x) => Right((x, chart))
        case None => Left(OutputFileNotFound(pair))
      }
    }

    Util.mapLeft(Util.accEithers(in.toSet.map(getOutputFile))) { errs =>
      new OutputFileErrors(errs.toSeq.map(_.toString))
    }
      //add the extension
      .map(_.toMap.map {
        case (key, value) => (addExtension(outputExtension)(key), value)
      })
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
      val collectedErrs = allErrors.foldLeft(Seq(): Seq[String]) {
        case (acc, (thisFile, errs)) => {
          val msg = s"Errors for $thisFile: $errs"
          acc :+ msg
        }
      }

      Left(new OutputFileErrors(collectedErrs))
    }
  }

  private def writeCharts(charts: Map[File, JFreeChart]): Either[SimError, Seq[File]] = {
    ???
  }

  /**
    * add the extension to file if it doesn't already exist
    * @param extension
    * @param f
    * @return
    */
  private def addExtension(extension: String)(f: File): File = {
    val newPath =
      if(f.getPath.endsWith(extension)) {
        f.getPath + extension
      } else {
        f.getPath
      }

    new File(newPath)
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
    extends SimError(msg) {
    def this(errs: Seq[String]) {
      this("Errors in output files: " +
        com.jakway.term.interface.Formatter.formatSeqMultilineNoHeader(errs))
    }
  }

  case class OutputFileNotFound(forVariable: VariablePairChart)
    extends SimError(s"Could not find an output file " +
      s"for variable ${forVariable.xVarName}")
}
