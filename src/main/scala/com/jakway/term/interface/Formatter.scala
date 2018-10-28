package com.jakway.term.interface

import java.util.Locale

class Formatter {
  val sb = new StringBuffer()
  val fmt = new java.util.Formatter(sb, Locale.getDefault())

  def formatSeqMultiline[A](header: String,
                            afterHeader: String = "\n",
                            beforeEachElement: String = "\t",
                            afterEachElement: String = "\n",
                            footer: String = "")
                           (xs: Seq[A]): String = {

    //print the header
    fmt.format("%s%s", header, afterHeader)

    val lastIndex = xs.length - 1
    xs.zipWithIndex
      .foreach {
        case (x, index) => {
          fmt.format("%s%s", beforeEachElement, x.toString)
          if(index != lastIndex) {
            fmt.format("%s", afterEachElement)
          }
        }
      }

    fmt.format("%s", footer)

    //return everything we've formatted so far
    sb.toString
  }
}

object Formatter {
  /**
    * default line width to use if right-justifying
    */
  val defaultWidth: Int = 80
}
