package org.singingwizard.regexopt

trait PrecomputeHashcode { self: Product => 
  override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}
