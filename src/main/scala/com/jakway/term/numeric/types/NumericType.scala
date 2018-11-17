package com.jakway.term.numeric.types

import com.jakway.term.elements.{Literal, NumericTerm}
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.errors.CouldNotReadLiteralError
import com.jakway.term.numeric.types.SpecialLiterals.{HasSpecialLiterals, SpecialLiteral, SpecialLiteralNotImplementedError, SpecialLiteralReadErrors}
import com.jakway.term.numeric.types.implementations.DoublePrecision

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


trait NumericType[M] {
  import NumericType._
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
  val multiply: BinaryMathFunction
  val divide: BinaryMathFunction

  val readLiteral: ReadLiteral[M]

  val builtinLiterals: BuiltinLiterals[M]
}

object NumericType {
  type ReadLiteral[M] = String => Either[SimError, M]

  object Implementations {
    def getDoublePrecisionNumericTypeImplementation():
      Either[SimError, NumericType[Double]] =
      DoublePrecision.mkNumericType
  }

}

class BuiltinLiterals[M](
  val negativeOne: M,
  val zero: M,
  val specialLiterals: Map[SpecialLiteral, M])
  extends HasSpecialLiterals[M]

object BuiltinLiterals {
  def mkBuiltinLiterals[M](
      readLiteral: String => Either[SimError, M]):
  Either[SimError, BuiltinLiterals[M]] = {
    for {
      negativeOne <- readLiteral("-1")
      zero <- readLiteral("0")
      specialLiterals <- readSpecialLiterals[M](readLiteral)
    } yield {
      new BuiltinLiterals(negativeOne, zero, specialLiterals)
    }
  }

  private def readSpecialLiterals[M](
       readLiteral: String => Either[SimError, M]):
    Either[SimError, Map[SpecialLiteral, M]] = {

    val empty: Either[Seq[SimError], Seq[(SpecialLiteral, M)]] =
      Right(Seq())

    val res = SpecialLiterals.Values.specialLiterals.foldLeft(empty) {
      case (acc, thisValue) => {
        readLiteral(thisValue.canonicalName) match {
          case Right(r) => acc match {
              //ignore success if we've already encountered an error
            case Left(es) => Left(es)
              //otherwise accumulate successes
            case Right(rs) => Right(rs :+ (thisValue, r))
          }
          case Left(e) => acc match {
            case Left(es) => Left(es :+ e)
              //start accumulating errors the first time
              //we encounter one
            case Right(rs) => Left(Seq(e))
          }
        }
      }
    }

    res match {
      case Right(xs) => Right(xs.toMap)
      case Left(es) => Left(SpecialLiteralReadErrors(es))
    }
  }
}

trait NumericTypeImplementationHelper[M] extends NumericType[M] {
  /**
    * helper method for functions that can't fail
    * @param f
    * @return
    */
  def total2(f: M => M): M => Either[SimError, M] = (x: M) => Right(f(x))
  def total3(f: M => M => M): M => M => Either[SimError, M] =
    (x: M) => (y: M) => Right(f(x)(y))
}

