package com.jakway.term.test.framework.gen

import org.scalacheck.Gen

object GenUtils {
  /**
    * ScalaCheck really needs an equivalent to Haskell's "return" function
    * to lift values into the type
    * @param x
    * @tparam A
    * @return
    */
  def wrap[A](x: A): Gen[A] = {
    Gen.alphaNumStr.map(_ => x)
  }

  /**
    * @param xs
    * @tparam A
    * @return a seq in random order
    */
  def scramble[A](genXs: Gen[Seq[A]]): Gen[Seq[A]] = {
    genXs.flatMap { xs =>
      val indices = xs.zipWithIndex.map(_._2)

      val genScrambledIndices: Gen[Seq[Int]] = {
        //(accumulated scrambled indices, remaining indices)
        var acc: Gen[(Seq[Int], Seq[Int])] = genXs.map(_ => (Seq(), indices))
        var stop: Boolean = false
        var numIt: Int = 0

        //build a random list of indexes (sample without replacement)
        while(!stop) {
          //sanity check to make sure we don't loop forever
          if(numIt > xs.length) {
            throw new GenError(s"too many loop iterations (possible infinite loop?)" +
              s" for scramble($genXs")
          }

          val newAcc = acc.flatMap { m =>
            val (currentAcc, remainingIndices) = m
            if(remainingIndices.isEmpty) {
              stop = true
              m
            } else {
              val genPick = Gen.oneOf(remainingIndices)

              genPick.map(p => (currentAcc :+ p, p)).map {
                case (rAcc, pick) => {
                  val rIndices = remainingIndices.filterNot(_ == pick)
                  (rAcc, rIndices)
                }
              }
            }

          }

          acc = newAcc
          numIt = numIt + 1
        }

        //discard remainingIndices
        acc.map(_._1)
      }


      genScrambledIndices.map { scrambledIndices =>
        //take each index from the original list
        scrambledIndices.map(xs(_))
      }
    }
  }
}
