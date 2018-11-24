package com.jakway.term.test.framework

import java.math.BigDecimal

object CompareNumbers {
  /**
    * returns the result of calling left.compareTo(right)
    * ought to work even for values that are very large or very small or differ in precision
    * @param a
    * @param b
    * @tparam A
    * @tparam B
    * @return
    */
  def compare[A, B](a: A, b: B): Int = {
    val res = new BigDecimal(a.toString).compareTo(new BigDecimal(b.toString))
    //check range of allowable compareTo values
    assert(res == -1 || res == 0 || res == 1)
    res
  }

  def apply[A, B] = compare[A, B] _
}
