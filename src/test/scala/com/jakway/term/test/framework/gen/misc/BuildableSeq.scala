package com.jakway.term.test.framework.gen.misc

import org.scalacheck.util.Buildable

import scala.collection.mutable.Builder

object BuildableSeq {
  implicit def buildableSeq[T]: Buildable[T, Seq[T]] = new Buildable[T, Seq[T]] {
    override def builder: Builder[T, Seq[T]] =
      new Builder[T, Seq[T]] {
        var s: Seq[T] = Seq()
        override def +=(elem: T): this.type = {
          s = s :+ elem
          this
        }

        override def clear(): Unit = {
          s = Seq()
        }

        override def result(): Seq[T] = s
      }
  }

}
