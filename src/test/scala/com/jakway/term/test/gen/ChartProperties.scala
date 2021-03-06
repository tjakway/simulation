package com.jakway.term.test.gen

import java.io.File
import java.nio.file.Files

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.run.SimulationRun
import com.jakway.term.run.result.chart._
import com.jakway.term.test.framework.gen._
import com.jakway.term.test.gen.ChartProperties.ChartPropertiesSetupError
import org.jfree.chart.plot.PlotOrientation
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import scala.concurrent.{Future, Await}
import scala.util.{Failure, Success, Try}

trait ChartProperties[N <: NumericType[M], M]
  extends SimulationRunPropertiesHelper[N, M]
    with HasInterpreter[N, M]
    with HasNumericType[N, M]
    with CheckRun
    with BasePropertiesTrait {
  this: Properties =>

  val maxNumCharts: Option[Int] = Some(5)

  //WARNING: will throw on class initialization
  checkMaxNumCharts() match {
    case Left(t) => throw t
    case _ => {}
  }

  //5 seconds
  val timeOutMillis: Long = 5000


  case class SimulationRunWithCharts(simulationRun: SimulationRun,
                                     chartConfig: ChartConfig,
                                     writeChartsConfig: WriteChartsConfig)

  def genWriteChartsConfig(forCharts: Seq[String]): Either[SimError, Gen[WriteChartsConfig]] = {

    //set up a writeable temp dir with the +x bit set
    def mkTempDir(): Either[SimError, File] = {
      val tempDirPrefix: String = "genwritechartsconfig"
      for {
        dir <- Try(Files.createTempDirectory(tempDirPrefix)) match {
          case Success(x) if x == null =>
            Left(ChartPropertiesSetupError(s"Files.createTempDirectory returned null for " +
              s"prefix = $tempDirPrefix"))
          case Success(x) => Right(x)
          case Failure(t) => Left(new ChartPropertiesSetupError(t))
        }

        _ <- if(!dir.toFile.canWrite) {
              if(!dir.toFile.setWritable(true)) {
                Left(ChartPropertiesSetupError(s"Cannot set $dir writeable"))
              } else { Right({}) }
            } else { Right({}) }

        _ <- if(!dir.toFile.canExecute) {
              if(!dir.toFile.setExecutable(true)) {
                Left(ChartPropertiesSetupError(s"Cannot set $dir executable"))
              } else { Right({}) }
            } else { Right({}) }

      } yield {
        dir.toFile
      }
    }

    //use that temp dir to initialize an instance of WriteChartsConfig
    mkTempDir().map { tempDir =>

      lazy val files = forCharts.map(c => (c, new File(tempDir, c))).toMap
      Gen.oneOf(ToDirectory(tempDir), ToFiles(files))
    }

  }

  def genSimulationRunWithCharts(maxCharts: Option[Int]): Gen[SimulationRunWithCharts] = Gen lzy {
    val genSimRun: Gen[SimulationRun] = getGenSimulationRun()
    val genDyVariables: Gen[Seq[String]] = genSimRun.map(_.dynamicVariables.toSeq)
    def genBool: Gen[Boolean] = Gen.oneOf(true, false)
    val genChartConfig: Gen[(SimulationRun, ChartConfig)] = for {
      simulationRun <- genSimRun
      mustExcludeVariables <- genBool
      failOnIncompleteData <- genBool
      allowEmptyCharts <- genBool
      dynamicVariables <- GenUtils.scramble(genDyVariables)

      //chart configuration pieces
      titles <- Gen.listOfN(dynamicVariables.size, Gen.alphaNumStr)
      xLabels <- Gen.listOfN(dynamicVariables.size, Gen.option(Gen.alphaNumStr))
      yLabels <- Gen.listOfN(dynamicVariables.size, Gen.option(Gen.alphaNumStr))
      orientations <- Gen.listOfN(dynamicVariables.size,
        Gen.oneOf(PlotOrientation.VERTICAL, PlotOrientation.HORIZONTAL))
      legends <- Gen.listOfN(dynamicVariables.size, genBool)
      tooltips <- Gen.listOfN(dynamicVariables.size, genBool)
      urls <- Gen.listOfN(dynamicVariables.size, genBool)

    } yield {
      val (makeChartsFor, dynamicsToExclude) =
        partitionVariables(maxCharts, simulationRun.dynamicVariables)

      val excludeVariables: Set[String] = simulationRun.constants ++ dynamicsToExclude

      //a list of generated chart configurations
      type ChartData = Seq[(String, Option[String], Option[String], PlotOrientation,
        Boolean, Boolean, Boolean)]
      val chartData: ChartData = {
        val empty: ChartData = Seq()
        dynamicVariables.zipWithIndex.map(_._2).foldLeft(empty) {
          case (acc, i) => {
            acc :+ (titles(i), xLabels(i), yLabels(i), orientations(i),
              legends(i), tooltips(i), urls(i))
          }
        }
      }

      val charts: Set[VariablePairChart] =
        chartData
          .take(makeChartsFor.size)
          .zip(makeChartsFor)
          .map {
            case ((title, xLabel, yLabel, orientation, legend, tooltip, url), varName) => {
              VariablePairChart(title, varName, simulationRun.outputVariable,
                xLabel, yLabel, orientation, legend, tooltip, url)
            }
          }
          .toSet


      val chartConfig = ChartConfig(excludeVariables, charts, interpreter.convertToNumber _,
        mustExcludeVariables, failOnIncompleteData, allowEmptyCharts)

      (simulationRun, chartConfig)
    }

    genChartConfig.flatMap {
      case (simulationRun, chartConfig) => {
        genWriteChartsConfig(chartConfig.charts.map(_.xVarName).toSeq)
          .map(_.map( writeChartsConfig =>
            SimulationRunWithCharts(simulationRun, chartConfig, writeChartsConfig)))
      }
        //throw errors
    }.flatMap { e =>
      e match {
        case Right(genX) => genX
        case Left(e) => throw e
      }
    }
  }

  implicit val arbSimulationRunWithCharts: Arbitrary[SimulationRunWithCharts] =
    Arbitrary(genSimulationRunWithCharts(maxNumCharts))

  property("generate charts from a SimulationRun") =
    forAll { (simulationRunWithCharts: SimulationRunWithCharts) =>
      val simulationRun = simulationRunWithCharts.simulationRun
      val chartConfig = simulationRunWithCharts.chartConfig
      val writeChartsConfig = simulationRunWithCharts.writeChartsConfig

      lazy val chartProcessor = new ChartProcessor(chartConfig)
      lazy val writeCharts = new WriteCharts(writeChartsConfig)

      import com.jakway.term.run.result.GenericResultProcessor._
      val res =
        mapResults(
          mapResults(Future(SimulationRun.run(simulationRun, interpreter)), chartProcessor.apply),
            writeCharts.apply)


      //for millis
      import scala.concurrent.duration._
      import scala.language.postfixOps
      checkRun(Await.result(res, timeOutMillis millis)).isRight
    }

  private def checkMaxNumCharts(): Either[SimError, Option[Int]] = maxNumCharts match {
    case Some(x) if x < 0 =>
      Left(ChartPropertiesSetupError(s"maxNumCharts is $x (expected >=0)"))

    case Some(x) if x == 0 => {
      logger.warn("maxNumCharts == 0 (ChartProperties skipped!)")
      Right(Some(x))
    }

    case y => Right(y)
  }

  /**
    *
    * @param maxCharts
    * @return (variables to make charts for, variables to exclude)
    */
  private def partitionVariables(maxCharts: Option[Int], dynamicVariables: Set[String]):
    (Set[String], Set[String]) = {

    maxCharts match {
      case Some(max) => dynamicVariables.splitAt(max)
      case None => (dynamicVariables, Set())
    }
  }
}

object ChartProperties {
  case class ChartPropertiesSetupError(override val msg: String)
    extends GenError(msg) {
    def this(t: Throwable) {
      this(s"Error caused by $t")
    }
  }

}
