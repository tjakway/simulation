package com.jakway.term.interpreter

import com.jakway.term.elements.Term
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.numeric.types.{NumericType, SimError}

/**
  * An optimization that stores previously evaluated terms
  * in a map for fast lookup
  * Differs from the symbol table in that it ignores scope and will
  * not reprocess terms that otherwise would be re-run
  * (e.g. log(x) will not actually call log(x) again)
  * @tparam N
  * @tparam M
  */
class Memoization[N <: NumericType[M], M] extends Optimization {
  import java.util.AbstractMap
  import java.util.concurrent.ConcurrentHashMap

  val evaluatedTerms: AbstractMap[Term, Term] =
    new ConcurrentHashMap[Term, Term]()

  def lookup(t: Term): Option[Term] =
    Option(evaluatedTerms.get(t))

  override def wrap(originalInterpreter: Interpreter): Interpreter = {
    new Interpreter {
      override def eval(table: SymbolTable)(t: Term):
        Either[SimError, Term] = {

        lookup(t) match {
            //check if we already evaluated this term
          case Some(x) => Right(x)
          case None => {
            //evaluate the term in the original interpreter,
            //adding it to the map if successful
            originalInterpreter.eval(table)(t).map {
              evaluatedTerm =>
                evaluatedTerms.put(t, evaluatedTerm)
                evaluatedTerm
            }
          }
        }
      }
    }
  }
}
