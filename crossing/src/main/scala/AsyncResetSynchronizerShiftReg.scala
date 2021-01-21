package org.chipsalliance.utils.crossing

import chisel3.util.Cat
import chisel3.withReset

// Note: This module may end up with a non-AsyncReset type reset.
// But the Primitives within will always have AsyncReset type.
class AsyncResetSynchronizerShiftReg(w: Int = 1, sync: Int, init: Int) extends AbstractPipelineReg(w) {
  require(sync > 1, s"Sync must be greater than 1, not ${sync}.")
  override def desiredName =
    s"AsyncResetSynchronizerShiftReg_w${w}_d${sync}_i${init}"
  val output = Seq.tabulate(w) { i =>
    val initBit = ((init >> i) & 1) > 0
    withReset(reset.asAsyncReset) {
      SynchronizerPrimitiveShiftReg(
        io.d(i),
        sync,
        initBit,
        SynchronizerResetType.Async
      )
    }
  }
  io.q := Cat(output.reverse)
}

object AsyncResetSynchronizerShiftReg {
  def apply[T <: Chisel.Data](
    in:   T,
    sync: Int,
    init: Int,
    name: Option[String] = None
  ): T =
    AbstractPipelineReg(
      new AsyncResetSynchronizerShiftReg(in.getWidth, sync, init),
      in,
      name
    )

  def apply[T <: Chisel.Data](in: T, sync: Int, name: Option[String]): T =
    apply(in, sync, 0, name)

  def apply[T <: Chisel.Data](in: T, sync: Int): T =
    apply(in, sync, 0, None)

  def apply[T <: Chisel.Data](
    in:   T,
    sync: Int,
    init: T,
    name: Option[String]
  ): T =
    apply(in, sync, init.litValue.toInt, name)

  def apply[T <: Chisel.Data](in: T, sync: Int, init: T): T =
    apply(in, sync, init.litValue.toInt, None)
}
