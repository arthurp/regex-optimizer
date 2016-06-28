package org.singingwizard

package object regexopt {
  def toFixedPoint[T](s: T, max: Int = Int.MaxValue)(f: T => T): T = {
    var v = s
    for (_ <- 0 until max) {
      val n = f(v)
      if (n == v) return v
      v = n
    }
    return v
  }
}