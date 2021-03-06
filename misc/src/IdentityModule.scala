package org.chipsalliance.utils.misc

import chisel3._
class IdentityModule[T <: Data](gen: T) extends MultiIOModule {
  val in:  T = IO(Flipped(gen.cloneType))
  val out: T = IO(gen.cloneType)

  out := in
}

object IdentityModule {
  def apply[T <: Data](x: T): T = {
    val identity = Module(new IdentityModule(x))
    identity.in := x
    identity.out
  }
}
