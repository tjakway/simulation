package com.jakway.term.run.result.chart

import java.awt.Rectangle
import java.io._

import com.jakway.term.Util
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.run.result.chart.WriteCharts.{OutputFileErrors, OutputFileNotFound}
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGraphics2D
import org.jfree.chart.JFreeChart
import org.w3c.dom.DOMImplementation

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class WriteCharts(val config: WriteChartsConfig)
  extends ChartProcessor.ResultProcessor[WriteCharts.OutputType] {

  val outputExtension = ".svg"

  override def apply(results: ChartProcessor.OutputType)
                    (implicit ec: ExecutionContext): Future[Either[SimError, WriteCharts.OutputType]] = {
    Future {
      for {
        dests <- getDests(results)
        _ <- checkAllOutputFiles(dests.keySet)
        out <- writeCharts(dests)
      } yield out
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
    val drawChart = new DrawChart(config.encoding)

    val empty: (Seq[SimError], Seq[File]) = (Seq(), Seq())
    val (allErrors, writtenFiles) = charts.foldLeft(empty) {
      case ((errs, writtenFiles), (thisFile, thisChart)) => {
        drawChart(thisChart, thisFile) match {
          case Right(_) => (errs, writtenFiles :+ thisFile)
          case Left(e) => (errs :+ e, writtenFiles)
        }
      }
    }

    if(allErrors.isEmpty) {
      Right(writtenFiles)
    } else {
      Left(new OutputFileErrors(allErrors.map(_.toString)))
    }
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
  type OutputType = Seq[File]

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

class DrawChart(val encoding: String = "UTF-8") {

  case class DrawChartError(t: Throwable, chart: JFreeChart, file: File)
    extends SimError(s"Error while writing $chart to $file: $t")

  private def getBounds(svgGenerator: SVGGraphics2D): Rectangle = {
    val dimensions = svgGenerator.getSVGCanvasSize
    new Rectangle(0, 0, dimensions.width, dimensions.height)
  }

  private def write(svgGenerator: SVGGraphics2D, to: File): Try[Unit] = {
    var osToClose: Option[OutputStream] = None

    def close(): Unit = {
      //close the stream if it's open and ignore failures
      Try(osToClose.foreach(_.close()))
    }

    val res = Try {
      val os = new FileOutputStream(to)
      osToClose = Some(os)
      val out: Writer = new BufferedWriter(new OutputStreamWriter(os, encoding))

      svgGenerator.stream(out, true)
      out.flush()
    }

    close()
    res
  }

  private def draw(chart: JFreeChart): Try[SVGGraphics2D] = Try {

    //from http://dolf.trieschnigg.nl/jfreechart/
    val domImplementation: DOMImplementation =
      GenericDOMImplementation.getDOMImplementation()
    val doc = domImplementation.createDocument(null, "svg", null)
    val svgGenerator: SVGGraphics2D = new SVGGraphics2D(doc)



    chart.draw(svgGenerator, getBounds(svgGenerator))

    svgGenerator
  }

  def apply(chart: JFreeChart, to: File): Either[SimError, Unit] = {
    val res = for {
      svgGenerator <- draw(chart)
      _ <- write(svgGenerator, to)
    } yield {}

    res match {
      case Success(x) => Right(x)
      case Failure(t) => Left(DrawChartError(t, chart, to))
    }
  }
}
