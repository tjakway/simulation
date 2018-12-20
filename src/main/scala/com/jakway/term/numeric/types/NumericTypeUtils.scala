package com.jakway.term.numeric.types

import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericTypeUtil.IncrementingStreamError

class NumericTypeUtils[N <: NumericType[M], M](numericType: N) {

  /**
    * lazily construct a stream that proceeds by the passed step
    * @param from
    * @param to
    * @param step
    * @return
    */
  def incrementingStream(from: M, to: Option[M],
                         step: M = numericType.builtinLiterals.one): Stream[M] = {

    def incByStep(x: M): M =
      numericType.add(x)(step) match {
        case Right(q) => q
        case Left(e) =>
          throw IncrementingStreamError(s"Error incrementing $x by step $step: $e")
      }

    def helper(last: M, max: Option[M]): Stream[M] = {
      lazy val nextValue = incByStep(last)

      max match {
        case Some(stop) =>
          //break condition
          if(last == stop) {
            Stream.empty
          } else {
            nextValue #:: helper(nextValue, max)
          }
        case None => nextValue #:: helper(nextValue, max)
      }
    }

    from #:: helper(from, to)
  }

}

object NumericTypeUtil {
  case class IncrementingStreamError(override val msg: String)
    extends SimError(msg)
}
