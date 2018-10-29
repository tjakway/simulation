package com.jakway.term.elements

/**
  * Equation is intentionally not an instance of Term
  * @param left
  * @param right
  */
case class Equation(left: Term, right: Term) {

  def contains(t: Term): Boolean =
    left.contains(t) || right.contains(t)

  /**
    * compares terms using .matches
    * @param o
    * @return
    */
  override def equals(o: Any): Boolean = {
    def eq(e: Equation): Boolean = {
      left.matches(e.left) && right.matches(e.right)
    }

    o.isInstanceOf[Any] && eq(o.asInstanceOf[Equation])
  }
}
