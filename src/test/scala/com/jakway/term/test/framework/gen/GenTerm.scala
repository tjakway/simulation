package com.jakway.term.test.framework.gen

import com.jakway.term.elements._
import com.jakway.term.interpreter.{Interpreter, InterpreterResult, Raw}
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.NumericType
import com.jakway.term.test.framework.gen.GenTerm.{ConstantNumericTermEvalException, GenMError}
import org.scalacheck.Gen

/**
  * Need to use Gen.lzy to avoid stack overflows
  * see https://stackoverflow.com/a/19831105
  * @tparam N
  * @tparam M
  */
trait GenTerm[N <: NumericType[M], M]
  extends HasNumericType[N, M] {
  import GenLeaf._

  object GenLeaf {
    private lazy val otherTerms: Seq[Gen[Term]] = Seq(Gen.oneOf(Seq(IdentityFunction)))

    lazy val numericTermLeafPossibilities: Seq[Gen[NumericTerm[N, M]]] = Seq(genRaw, genLiteral, genVariable)
    lazy val genLeafPossibilities: Seq[Gen[Term]] = numericTermLeafPossibilities ++ otherTerms

    def genLeaf(possibilities: Seq[Gen[Term]] = genLeafPossibilities): Gen[Term] = {
      Gen.lzy(Gen.oneOf[Gen[Term]](possibilities).flatMap(x => x))
    }

    def genLeaf(variablesAllowed: Boolean): Gen[Term] =
      if(variablesAllowed) {
        genLeaf()
      } else {
        WithoutVariables.genLeaf()
      }

    def genNumericTermLeaf(
        possibilities: Seq[Gen[NumericTerm[N, M]]] = numericTermLeafPossibilities): Gen[NumericTerm[N, M]] =
      Gen.lzy(Gen.oneOf[Gen[NumericTerm[N, M]]](possibilities).flatMap(x => x))

    def genNumericTermLeaf(variablesAllowed: Boolean): Gen[NumericTerm[N, M]] =
      if(variablesAllowed) {
        genNumericTermLeaf()
      } else {
        WithoutVariables.genNumericTermLeaf()
      }

    object WithoutVariables {
      def isVariableGen[T](x: Gen[T]): Boolean = x == genVariable

      def genLeaf(): Gen[Term] = GenLeaf.genLeaf(
        genLeafPossibilities.filterNot(isVariableGen))

      def genNumericTermLeaf(): Gen[NumericTerm[N, M]] = GenLeaf.genNumericTermLeaf(
        numericTermLeafPossibilities.filterNot(isVariableGen))
    }
  }

  def genStr: Gen[String] = Gen.alphaNumStr

  val genVariable: Gen[Variable[N, M]] =
      for {
        name <- genStr
        desc <- genStr
        optDesc <- Gen.option(desc)
      } yield { Variable.apply(name, optDesc) }

  def genM: Gen[M] = Gen.numStr.suchThat(_.length > 0)
    .map(numericType.readLiteral(_) match {
      case Right(x) => x
      case Left(r) => throw GenMError(r)
    })

  def genMInRange(min: M, max: M): Gen[M] =
    genM.suchThat(x =>
      numericType.comparator.compare(x, min) >= 0 &&
      numericType.comparator.compare(x, max) <= 0)

  def genRaw: Gen[Raw[N, M]] = genM.map(Raw(_))

  def genNumericTerm(variablesAllowed: Boolean = true): Gen[NumericTerm[N, M]] = {
    Gen.lzy(Gen.oneOf(Gen.lzy(genNumericTermLeaf(variablesAllowed)),
      Gen.lzy(genNumericTermBranch(variablesAllowed))))
  }

  def genTerm(variablesAllowed: Boolean): Gen[Term] = {
    Gen.oneOf(Gen.const(IdentityFunction), genNumericTerm(variablesAllowed))
  }

  def genLiteral: Gen[Literal[N, M]] = genM.map(m => Literal(m.toString))


  /**
    * @param variablesAllowed
    * @param logarithmTreeInterpreter an optional interpreter to evaluate
    *                                 constant expressions to allow for better
    *                                 generated Logarithm expressions (since we need
    *                                 to restrict the generated trees to be > 0)
    *                                 Otherwise they will merely be Raw values
    * @return
    */
  def genNumericTermBranch(
     variablesAllowed: Boolean = true,
     logarithmTreeInterpreter: Option[Interpreter] = None):
    Gen[NumericTerm[N, M]] = {

    def genBinaryTerm: Gen[NumericTerm[N, M]] = Gen lzy {
      def genLogarithm: Gen[Logarithm[N, M]] = Gen lzy {
        def gtZero(r: Raw[N, M]): Boolean = {
          numericType.comparator.compare(r.value,
            numericType.builtinLiterals.zero) == 1
        }

        def neOne(r: Raw[N, M]): Boolean = {
          numericType.comparator.compare(r.value,
            numericType.builtinLiterals.one) != 0
        }

        def genGtZero(interpreter: Interpreter): Gen[NumericTerm[N, M]] = {

          def filter(res: InterpreterResult): Boolean = {
            res.isInstanceOf[Raw[N@unchecked, M@unchecked]] &&
              gtZero(res.asInstanceOf[Raw[N@unchecked, M@unchecked]])
          }

          genConstantNumericTerm(interpreter, filter)
        }

        /**
          * >0 && != 1
          * @param interpreter
          * @return
          */
        def genGtZeroNeOne(interpreter: Interpreter): Gen[NumericTerm[N, M]] = {
          genGtZero(interpreter)
            .filter(x => x.isInstanceOf[Raw[N, M]] && neOne(x.asInstanceOf[Raw[N, M]]))
        }

        /**
          * if we have an interpreter to evaluate generated
          * constant expressions then we can pass a full tree
          * to the logarithm
          * otherwise we merely restrict the generated Raw value
          *
          * see https://math.stackexchange.com/q/2073219
          * re: the name of the Logarithm argument
          */
        def genLogarithmArgument(): Gen[NumericTerm[N, M]] =
          logarithmTreeInterpreter match {
            case Some(interpreter) => genGtZero(interpreter)
            case None => genRaw.filter(gtZero)
          }

        def genLogarithmBase(): Gen[NumericTerm[N, M]] =
          logarithmTreeInterpreter match {
            case Some(interpreter) => genGtZeroNeOne(interpreter)
            case None => genRaw.filter(r => gtZero(r) && neOne(r))
          }


        for {
          base <- genLogarithmBase()
          of <- genLogarithmArgument()
        } yield Logarithm(base, of)
      }

      //can't include Logarithm in the list of binary functions
      //because it has domain restrictions
      val binaryFunctions:
        Seq[(NumericTerm[N, M], NumericTerm[N, M]) => NumericTerm[N, M]] =
        Seq(Add.apply, Subtract.apply, Multiply.apply, Divide.apply,
          Power.apply)
      val binaryTerm = for {
        left <- genNumericTerm(variablesAllowed)
        right <- genNumericTerm(variablesAllowed)
        f <- Gen.oneOf(binaryFunctions)
      } yield {
        f(left, right)
      }

      Gen.oneOf(binaryTerm, genLogarithm)
    }


    def genTrigFunction: Gen[NumericTerm[N, M]] = Gen lzy {

      type TrigF = NumericTerm[N, M] => NumericTerm[N, M]
      val unrestrictedTrigFunctions: Seq[(TrigF, Gen[NumericTerm[N, M]])] =
        Seq(Sin.apply[N, M](_), Cos.apply[N, M](_), Arctan.apply[N, M](_))
          .map(f => (f, genNumericTerm(variablesAllowed)))

      def doubleToNumericTerm(x: Double) =
        Raw.apply[N, M](numericType.readLiteral(x.toString).right.get)

      val restrictedTrigFunctions: Seq[(TrigF, Gen[NumericTerm[N, M]])] =
        Seq(
          (Arcsin.apply[N, M](_), Gen.choose(0: Double, 1: Double)),
          (Arccos.apply[N, M](_), Gen.choose(0: Double, 1: Double)),
          //tan isn't defined for any x where cos(x) == 0
          //also manually checked that java.lang.Math.cos works for MinValue and MaxValue
          (Arctan.apply[N, M](_), Gen.choose(Double.MinValue, Double.MaxValue)
                                     .filter(java.lang.Math.cos(_) != 0))
        ).map {
          case (f, g) => (f, g.map(doubleToNumericTerm))
        }

      val allTrigFunctions: Seq[(TrigF, Gen[NumericTerm[N, M]])] =
        unrestrictedTrigFunctions ++ restrictedTrigFunctions


      for {
        (function, gen) <- Gen.oneOf(allTrigFunctions)
        arg <- gen
      } yield function(arg)
    }

    def genNegative: Gen[Negative[N, M]] =
      Gen.lzy(genNumericTerm(variablesAllowed).map(Negative.apply))

    Gen.lzy(Gen.oneOf(genNegative, genBinaryTerm, genTrigFunction))
  }

  def genConstantNumericTerm(): Gen[NumericTerm[N, M]] = genNumericTerm(false)
  def genConstantNumericTerm(interpreter: Interpreter,
                             filter: InterpreterResult => Boolean): Gen[NumericTerm[N, M]] = {
    genConstantNumericTerm().filter { term =>
      interpreter.eval(Map())(term) match {
        case i: InterpreterResult => filter(i)
        case res => throw ConstantNumericTermEvalException(
          s"Interpreter $interpreter " +
          s"eval($term) returned $res (expected instance of" +
          s" InterpreterResult)")
      }
    }
  }
}

object GenTerm {
  case class ConstantNumericTermEvalException(override val msg: String)
    extends GenError(msg)

  case class GenMError(val causedBy: SimError)
    extends GenError(s"Error in genM caused by $causedBy")
}
