package com.jakway.term.numeric.types

import com.jakway.term.numeric.errors.SimError

/**
  * literals that need special handling
  * famous constants like e, pi, etc.
  * these typically already have specific backing
  * by the underling numeric type
  */
object SpecialLiterals {
  class SpecialLiteral(val canonicalName: String,
                       val otherNames: Set[String]) {
    def this(name: String) = this(name, Set(name))

    val allNames: Set[String] = otherNames + canonicalName
    /**
      * returns true if the argument is
      * this SpecialLiteral or if it is any of the names
      * of this SpecialLiteral
      * @param o
      * @return
      */
    override def equals(o: Any): Boolean = {
      def eqString(s: String): Boolean = {
        allNames.contains(s)
      }

      def eqOther(other: SpecialLiteral): Boolean =
        allNames == other.allNames

      ( o.isInstanceOf[String] &&
        eqString(o.asInstanceOf[String]) ) ||
        ( o.isInstanceOf[SpecialLiteral]
            && eqOther(o.asInstanceOf[SpecialLiteral]))
    }

    /**
      * whether the parameter is a name of this special literal
      * @param n
      * @return
      */
    def isName(n: String): Boolean = allNames.contains(n)
  }

  object Values {
    val e = new SpecialLiteral("e")
    val pi =
    //see https://en.wikipedia.org/wiki/Pi_(letter)
    //for unicode representations of pi
      new SpecialLiteral("pi", Set(
        //"\uD835\uDF0B" is unicode 'MATHEMATICAL ITALIC SMALL PI'
        //https://www.fileformat.info/info/unicode/char/1d70b/index.htm
        "\uD835\uDF0B",
        //'MATHMETICAL SANS-SERIF BOLD SMALL PI'
        "\uD835\uDF7F"))


    val specialLiterals: Set[SpecialLiteral] = Set(
      e,
      pi
    )

    /**
      * @param lit
      * @return true if this is a name of any special literal
      */
    def contains(lit: String): Boolean =
      specialLiterals.find(_.allNames.contains(lit))
        .isDefined
  }

  /**
    * a trait that makes using special literals easier
    * @tparam N
    * @tparam M
    */
  trait HasSpecialLiterals[M] {
    val specialLiterals: Map[SpecialLiteral, M]
    private def lookupSpecLiteral(s: SpecialLiteral): M = {
      specialLiterals.get(s).get
    }

    val e: M = lookupSpecLiteral(Values.e)
    val pi: M = lookupSpecLiteral(Values.pi)
  }

  def contains(lit: String): Boolean =
    Values.contains(lit)


  class SpecialLiteralError(override val msg: String)
    extends SimError(msg)


  case class SpecialLiteralNotImplementedError(lit: String)
    extends SpecialLiteralError(s"$lit was identified as a special literal" +
      s" but its value was not properly assigned" +
      s" (there was an error in a NumericType implementation)")

  case class SpecialLiteralReadErrors(errors: Seq[SimError])
    extends SpecialLiteralError("Encountered these errors " +
      s"while reading special literals: $errors")
}
