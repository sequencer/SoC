package org.chipsalliance.utils.crossing

import chisel3._

final class RationalIO[T <: Data](gen: T) extends Bundle {
  val bits0 = Output(gen)
  val bits1 = Output(gen)
  val valid = Output(Bool())
  val source = Output(UInt(2.W))
  val ready = Input(Bool())
  val sink = Input(UInt(2.W))

  override def cloneType: this.type =
    new RationalIO(gen).asInstanceOf[this.type]
}

object RationalIO {
  def apply[T <: Data](gen: T) = new RationalIO(gen)
}
