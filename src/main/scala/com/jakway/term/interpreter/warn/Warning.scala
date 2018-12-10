package com.jakway.term.interpreter.warn

import java.util.Locale

import com.jakway.term.elements.{Equation, Term}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.solver.Solvable


/**
  * Type that wraps things we warn about
  */
sealed trait WarningObject {
  def warningFor: String
}
case class TermWarning(t: Term) extends WarningObject {
  override def warningFor: String = t.toString
}
case class SolvableWarning(solvable: Solvable) extends WarningObject {
  override def warningFor: String = solvable.toString
}

class Warning(val obj: WarningObject, val warning: String)
  extends SimError(
    s"Warning for ${obj.warningFor}: $warning") {

  /**
    * convenience constructors
    *
    * @param t
    * @param warning
    * @return
    */
  def this(t: Term, warning: String) =
    this(TermWarning(t), warning)

  def this(solvable: Solvable, warning: String) =
    this(SolvableWarning(solvable), warning)

}

object Warning {
  /**
    * print warnings grouped by object
    * @param warnings
    * @return
    */
  def formatWarnings(warnings: Seq[Warning]): String = {
    import java.util.Formatter
    val sb: StringBuffer = new StringBuffer()
    val formatter: Formatter = new Formatter(sb, Locale.getDefault())

    warnings.groupBy(_.obj).foreach {
      case (thisTerm, termWarnings) => {
        //note: java.util.Formatter converts \n to the platform-
        //specific line separator
        formatter.format("Warnings for %s:\n", thisTerm.warningFor)
        termWarnings.foreach(w =>
          formatter.format("\t%s\n", w))
      }
    }

    sb.toString
  }
}
