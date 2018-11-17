package com.jakway.term.interpreter.helpers

import com.jakway.term.elements._
import com.jakway.term.interpreter.Eval.{EvalHelper, EvalType}
import com.jakway.term.interpreter.{Interpreter, Raw}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.NumericType

class EvalTrigFunctions[N <: NumericType[M], M](
  val recurse: Interpreter,
  val numericType: N)
  extends EvalHelper[TrigFunction[N, M]](recurse) {
  import com.jakway.term.interpreter.Raw._

  override def apply(table: SymbolTable)(f: TrigFunction[N, M]): EvalType =
  f match {

      //type inference fails here for reasons unknown (Raw(x) should infer x as M)
    case Sin(Raw(x)) => raw(numericType.sin(x.asInstanceOf[M]))
    case Cos(Raw(x)) => raw(numericType.cos(x.asInstanceOf[M]))
    case Tan(Raw(x)) => raw(numericType.tan(x.asInstanceOf[M]))
    case Arcsin(Raw(x)) => raw(numericType.arcsin(x.asInstanceOf[M]))
    case Arccos(Raw(x)) => raw(numericType.arccos(x.asInstanceOf[M]))
    case Arctan(Raw(x)) => raw(numericType.arctan(x.asInstanceOf[M]))
  }
}
