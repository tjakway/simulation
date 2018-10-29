package com.jakway.term.elements

import com.jakway.term.elements.HasSubterms.NewInstanceF
import com.jakway.term.numeric.types.SimError

import scala.reflect.ClassTag

object HasSubterms {
  type NewInstanceF = Seq[Term] => Term


  case class NewInstanceException(override val msg: String)
    extends SimError(msg)

  def assertArity(arity: Int, subterms: Seq[Term]): Seq[Term] =
    if(subterms.length != arity) {
      throw new NewInstanceException(s"Expected arity of $arity" +
        s" but got new subterms: $subterms")
    } else subterms

  def assertCast[A <: Term](t: Term)(implicit tag: ClassTag[A]): A = {
    try {
      t.asInstanceOf[A]
    } catch {
      case e: Throwable => {
        val newException = NewInstanceException(s"Error while casting $t " +
          s"to " + tag.runtimeClass.getName)
        newException.addSuppressed(e)
        throw newException
      }
    }
  }



  def unapply(h: HasSubterms): Option[Seq[Term]] =
    Some(h.subterms)
}

trait HasSubterms extends Term {
  val subterms: Seq[Term]

  def contains(t: Term): Boolean = equals(t) || subterms.contains(t)
  def newInstance: NewInstanceF


  override def matches(other: Term): Boolean = {
    sameType(other) &&
      subterms == other.asInstanceOf[HasSubterms].subterms
  }
}