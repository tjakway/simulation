package com.jakway.term.test.framework.gen

import com.jakway.term.TermOperations
import com.jakway.term.elements.{Term, Variable}
import com.jakway.term.interpreter.Interpreter.SymbolTable
import com.jakway.term.interpreter.Raw
import com.jakway.term.numeric.errors.SimError
import com.jakway.term.numeric.types.{NumericType, NumericTypeUtils}
import com.jakway.term.run.SimulationRun.ValueStreams
import com.jakway.term.run.{Combinations, ComputeValues, SimulationRun}
import com.jakway.term.solver.Solvable
import com.jakway.term.test.framework.gen.GenSimulationRun._
import org.scalacheck
import org.scalacheck.{Arbitrary, Gen}

trait GenSimulationRun[N <: NumericType[M], M]
  extends HasNumericType[N, M] {
  private val outerNumericType: N = numericType

  private val genSolvable: GenSolvable[N, M] =
    new GenSolvable[N, M](numericType)

  private val genTerm: GenTerm[N, M] = new GenTerm[N, M] {
    override val numericType: N = outerNumericType
  }

  //************************************************
  //config values
  //************************************************
  lazy val minVariableRange: Either[SimError, M] = {
    numericType
      .min
      .map(Right(_): Either[SimError, M])
      .getOrElse(numericType.readLiteral(NumericType.AllTypes.smallestMinOfAllTypes))
  }

  lazy val maxVariableRange: Either[SimError, M] = {
    numericType
      .max
      .map(Right(_): Either[SimError, M])
      .getOrElse(numericType.readLiteral(NumericType.AllTypes.smallestMaxOfAllTypes))
  }

  lazy val maxVariableSpread: Either[SimError, M] = {
    numericType
      .readLiteral("50")
  }
  //************************************************

  /**
    * @param maxNumConstants
    * @param maxNumDynamicVariables must be > 0
    */
  def genSimulationRun(allowConstantTerms: Boolean,
                       maxNumConstants: Option[Int] = None,
                       maxNumDynamicVariables: Option[Int] = None):
    Either[SimError, Gen[SimulationRun]] = {

    //parameter checks
    for {
      maxConstants <- checkMaxNumConstants(maxNumConstants)
      maxDynamic <- checkMaxNumDynamicVariables(maxNumDynamicVariables)

      minVarRange <- minVariableRange
      maxVarRange <- maxVariableRange
      maxVarSpread <- maxVariableSpread
    } yield {
      Gen lzy genSolvable.genSolved()
        .filter(x => withinVariableLimits(maxNumConstants, maxNumDynamicVariables)(x._2))
        .flatMap { x =>
          val (outputVar, eq) = x
          partitionVariables(maxConstants, maxDynamic)(eq)
            .map(vars => (outputVar, eq, vars))
        }
        .flatMap {
          case (outputVariable, solvable, partitionedVariables: PartitionedVariables[N, M]) => {
            val empty: Gen[Set[(Variable[N, M], M)]] = GenUtils.wrap(Set())
            val genConstantsSet: Gen[Set[(Variable[N, M], M)]] = partitionedVariables.constants
              .foldLeft(empty) {
                case (acc, thisVar) =>
                  acc.flatMap { accSet =>
                    genTerm.genMInRange(minVarRange, maxVarRange)
                      .map(thisConstant => {
                        val newElem = (thisVar, thisConstant)
                        accSet + newElem
                      })
                  }
            }

            val genConstants: Gen[SymbolTable] =
              genConstantsSet.map{ (cs: Set[(Variable[N, M], M)]) =>
                cs.map(x => (x._1.name, Raw.apply[N, M](x._2).asInstanceOf[Term]))
                  .toMap
              }

            val genDynamicRanges: Gen[ValueStreams] =
              //generate a range for each dynamic variable
              partitionedVariables.dynamics.map { thisDynamic =>
                val gLowerBound: Gen[M] = genTerm.genMInRange(minVarRange, maxVarRange)
                gLowerBound.map { (lowerBound: M) =>
                  val res: Either[SimError, M] = numericType.add(lowerBound)(maxVarSpread)
                  val upperBound = res match {
                    case Right(x) => x
                    case Left(e) => {
                      throw CalculateDynamicVariableRangeError(thisDynamic.name,
                        e)
                    }
                  }

                  //then turn that range into a stream
                  (thisDynamic.name, new NumericTypeUtils[N, M](numericType)
                    .incrementingStream(lowerBound, Some(upperBound),
                      numericType.builtinLiterals.one).map(Raw.apply[N, M](_)))
                }
                //and accumulate all of them in a Map
            }.foldLeft(GenUtils.wrap(Map()): Gen[ValueStreams]) {
                case (acc, next: Gen[(String, Stream[Raw[N, M]])]) => {
                  for {
                    thisMap <- acc
                    thisItem <- next
                  } yield {
                    thisMap.updated(thisItem._1, thisItem._2)
                  }
                }
              }



            val res: Gen[SimulationRun] = for {
              constants <- genConstants
              dynamicRanges <- genDynamicRanges
            } yield {
              new SimulationRun(dynamicRanges,
                outputVariable.name, solvable, computeValues(constants))
            }
            res
          }
        }
      }
  }

  private def computeValues(constants: SymbolTable): ComputeValues =
    new Combinations(constants)


  private def checkMaxNumConstants(maxNumConstants: Option[Int]):
    Either[SimError, Option[Int]] = {
    maxNumConstants match {
      case Some(x) if x < 0 => Left(NumConstantsError(x))
      case _ => Right(maxNumConstants)
    }
  }

  private def checkMaxNumDynamicVariables(maxNumDynamicVariables: Option[Int]):
  Either[SimError, Option[Int]] = {
    maxNumDynamicVariables match {
      case Some(x) if x <= 0 => Left(NumDynamicVariablesError(x))
      case _ => Right(maxNumDynamicVariables)
    }
  }

  /**
    * if there are limits on the number of variables, return true if this Solvable
    * is within them
    * @param maxNumConstants
    * @param maxNumDynamicVariables
    * @param solvable
    * @return
    */
  private def withinVariableLimits(maxNumConstants: Option[Int] = None,
                                   maxNumDynamicVariables: Option[Int] = None)
                                  (solvable: Solvable): Boolean = {
    maxNumConstants.flatMap { c =>
      maxNumDynamicVariables.map { d =>
        val numVariables = TermOperations.findVariables(solvable.inputSide).size
        numVariables <= (c + d)
      }
    }.getOrElse(true)
  }


  private def partitionVariables(maxNumConstants: Option[Int] = None,
                                 maxNumDynamicVariables: Option[Int] = None)
                                (solvable: Solvable): Gen[PartitionedVariables[N, M]] = {
    val empty: Gen[(Set[Variable[N, M]], Set[Variable[N, M]])] =
      GenUtils.wrap((Set(), Set()))

    def setIsFull(max: Option[Int])(s: Set[Variable[N, M]]): Boolean =
      max.map(s.size >= _).getOrElse(false)

    def constantsFull: Set[Variable[N, M]] => Boolean =
      setIsFull(maxNumConstants)

    def dynamicsFull: Set[Variable[N, M]] => Boolean =
      setIsFull(maxNumDynamicVariables)

    TermOperations
      .findVariables(solvable.inputSide)
      .toSet
      .foldLeft(empty) {
        case (g, thisVar) => {
          g.flatMap {
            case (constants, dynamics) => {
              //we can assign a variable to either be constant (generated once
              //and used across every simulation run) or dynamic (generated
              //on each run)
              //check if either set is full (which forces our choice)
              //otherwise, randomly decide

              val cFull = constantsFull(constants)
              val dFull = dynamicsFull(dynamics)

              //the random bool is only used if neither set is full
              Gen.oneOf(true, false).map { randAssignToConstants =>
                //fix type inference
                lazy val v = thisVar.asInstanceOf[Variable[N, M]]
                lazy val assignToConstants = (constants + v, dynamics)
                lazy val assignToDynamics = (constants, dynamics + v)

                if(cFull && dFull) {
                  throw BothFullError(maxNumConstants, maxNumDynamicVariables,
                    constants, dynamics)
                } else if(cFull) {
                  assignToDynamics
                } else if(dFull) {
                  assignToConstants
                } else {
                  if(randAssignToConstants) {
                    assignToConstants
                  } else {
                    assignToDynamics
                  }
                }

              }
            }
          }
        }
      }
      .map(x => PartitionedVariables.apply[N, M](x._1, x._2))

  }
}

object GenSimulationRun {
  case class NumDynamicVariablesError(maxNumDynamicVariables: Int)
    extends GenError("Expected maxNumDynamicVariables to be >0, " +
      " but got " + maxNumDynamicVariables.toString)

  case class NumConstantsError(maxNumConstants: Int)
    extends GenError("Expected maxNumConstants to be >=0, " +
      " but got " + maxNumConstants.toString)

  case class BothFullError[N <: NumericType[M], M]
                          (maxConstants: Option[Int], maxDynamics: Option[Int],
                           constants: Set[Variable[N, M]], dynamics: Set[Variable[N, M]])
    extends GenError(s"Both constants and dynamic " +
      s"variables are full (should never happen): maxConstants = $maxConstants, " +
      s"maxDynamics = $maxDynamics, constants = $constants, dynamics = $dynamics")


  case class CalculateDynamicVariableRangeError(name: String, e: SimError)
    extends GenError(s"Error while calculating range of dynamic variable" +
      s" $name: $e")

  case class PartitionedVariables[N <: NumericType[M], M](
                                  constants: Set[Variable[N, M]],
                                  dynamics: Set[Variable[N, M]]) {

    /**
      * apply a function to every variable
      * @param f
      * @return
      */
    def mapAll(f: Variable[N, M] => Variable[N, M]): PartitionedVariables[N, M] = {
      val newConstants = constants.map(f)
      val newDynamics = dynamics.map(f)

      copy(constants = newConstants)
        .copy(dynamics = newDynamics)
    }
  }
}
