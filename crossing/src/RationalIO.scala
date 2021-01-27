package org.chipsalliance.utils.crossing

import chisel3._

final class RationalIO[T <: Data](gen: T) extends Bundle {
  val bits0:  T = Output(gen)
  val bits1:  T = Output(gen)
  val valid:  Bool = Output(Bool())
  val source: UInt = Output(UInt(2.W))
  val ready:  Bool = Input(Bool())
  val sink:   UInt = Input(UInt(2.W))

  override def cloneType: this.type =
    new RationalIO(gen).asInstanceOf[this.type]
}

object RationalIO {
  def apply[T <: Data](gen: T) = new RationalIO(gen)
}
