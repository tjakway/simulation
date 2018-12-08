package com.jakway.term.test.framework.gen

import com.jakway.term.test.framework.TestError

class GenError(override val msg: String)
  extends TestError(msg)
