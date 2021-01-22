package org.chipsalliance.utils.crossing

import chisel3.Bool
import chisel3.util.Cat

class ResetSynchronizerShiftReg(w: Int = 1, sync: Int, init: Int) extends AbstractPipelineReg(w) {
  require(sync > 1, s"Sync must be greater than 1, not $sync.")

  override def desiredName =
    s"ResetSynchronizerShiftReg_w${w}_d${sync}_i$init"

  val output: Seq[Bool] = Seq.tabulate(w) { i =>
    val initBit = ((init >> i) & 1) > 0
    SynchronizerPrimitiveShiftReg(
      io.d(i),
      sync,
      initBit,
      SynchronizerResetType.Inferred
    )
  }
  io.q := Cat(output.reverse)
}

object ResetSynchronizerShiftReg {
  def apply[T <: Chisel.Data](
    in:   T,
    sync: Int,
    init: Int,
    name: Option[String] = None
  ): T =
    AbstractPipelineReg(
      new ResetSynchronizerShiftReg(in.getWidth, sync, init),
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
