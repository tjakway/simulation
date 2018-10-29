package com.jakway.term.elements

trait CommutativeOperation extends Operation {
  /**
    * An iterator over every permutation of this operation's
    * arguments
    * @return
    */
  def permutations: Iterator[CommutativeOperation] =
    subterms.permutations
      .map(newInstance(_).asInstanceOf[CommutativeOperation])
}

object CommutativeOperation {
  def unapply(o: CommutativeOperation): Option[Seq[Term]] = Operation.unapply(o)
}