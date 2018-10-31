package com.jakway.term.numeric.types

import com.jakway.term.elements.Literal

class SimError(val msg: String)
  extends RuntimeException(msg) {

  def this(msg: String, t: Throwable) = {
    this(msg)
    addSuppressed(t)
  }

  def this(t: Throwable) {
    this(s"Caught throwable: $t")
  }
}

/**
  * literals that need special handling
  * famous constants like e, pi, etc.
  * these typically already have specific backing
  * by the underling numeric type
  */
object SpecialLiterals {
  class SpecialLiteral(val names: Set[String]) {
    def this(name: String) = this(Set(name))

    /**
      * returns true if the argument is
      * this SpecialLiteral or if it is any of the names
      * of this SpecialLiteral
      * @param o
      * @return
      */
    override def equals(o: Any): Boolean = {
      def eqString(s: String): Boolean = {
        names.contains(s)
      }

      def eqOther(other: SpecialLiteral): Boolean =
        names == other.names

      ( o.isInstanceOf[String] &&
        eqString(o.asInstanceOf[String]) ) ||
        ( o.isInstanceOf[SpecialLiteral]
            && eqOther(o.asInstanceOf[SpecialLiteral]))
    }
  }

  val e = new SpecialLiteral("e")
  val pi =
    //see https://en.wikipedia.org/wiki/Pi_(letter)
    //for unicode representations of pi
    new SpecialLiteral(Set("pi",
      //"\uD835\uDF0B" is unicode 'MATHEMATICAL ITALIC SMALL PI'
      //https://www.fileformat.info/info/unicode/char/1d70b/index.htm
      "\uD835\uDF0B",
       //'MATHMETICAL SANS-SERIF BOLD SMALL PI'
       "\uD835\uDF7F"))


  /**
    * @param lit
    * @return true if this is a name of any special literal
    */
  def contains(lit: String): Boolean =
    specialLiterals.find(_.names.contains(lit))
                   .isDefined

  val specialLiterals: Set[SpecialLiteral] = Set(
    e,
    pi
 )

  case class SpecialLiteralNotImplementedError(lit: String)
    extends SimError(s"$lit was identified as a special literal" +
      s" but its value was not properly assigned" +
      s" (there was an error in a NumericType implementation)")
}


trait NumericType[M] {
  type UnaryFunction = M => Either[SimError, M]
  type TrigFunction = UnaryFunction

  val sin: TrigFunction
  val cos: TrigFunction
  val tan: TrigFunction

  val arcsin: TrigFunction
  val arccos: TrigFunction
  val arctan: TrigFunction

  type BinaryMathFunction = M => M => Either[SimError, M]

  val pow: BinaryMathFunction
  val log: BinaryMathFunction

  val add: BinaryMathFunction
  val times: BinaryMathFunction
  val div: BinaryMathFunction

  val readLiteral: String => Either[SimError, M]
}

trait NumericTypeImplementation[M] extends NumericType[M] {
  /**
    * helper method for functions that can't fail
    * @param f
    * @return
    */
  def total2(f: M => M): M => Either[SimError, M] = (x: M) => Right(f(x))
  def total3(f: M => M => M): M => M => Either[SimError, M] =
    (x: M) => (y: M) => Right(f(x)(y))
}

