package com.jakway.term.test.framework.gen

import java.util.Locale

import com.jakway.term.numeric.errors.SimError

trait CheckRun extends BasePropertiesTrait {

  def reportFailure(msg: String): Unit = {
    logger.warn(msg)
  }

  def checkRun[R](res: Either[SimError, R]): Either[SimError, R] = {

    res match {
      case Left(e) =>  {
        val fmt = new java.util.Formatter(new StringBuffer(), Locale.getDefault())
        fmt.format(s"Scalacheck run failed:\t%s\n", e)
        reportFailure(fmt.toString)
      }
      case _ => {}
    }

    res
  }

}
