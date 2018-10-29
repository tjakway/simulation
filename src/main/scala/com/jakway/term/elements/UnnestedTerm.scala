package com.jakway.term.elements

/**
  * if a term doesn't contain any subterms then just
  * check if this is the term we're looking for
  */
trait UnnestedTerm extends Term {
  override def contains(t: Term): Boolean = equals(t)
}
