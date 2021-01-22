package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.Cat

// Note: This module may end up with a non-Bool type reset.
// But the Primitives within will always have Bool reset type.
@deprecated(
  "SyncResetSynchronizerShiftReg is unecessary with Chisel3 inferred resets. Use ResetSynchronizerShiftReg which will use the inferred reset type.",
  "rocket-chip 1.2"
)
class SyncResetSynchronizerShiftReg(w: Int = 1, sync: Int, init: Int) extends AbstractPipelineReg(w) {
  require(sync > 1, s"Sync must be greater than 1, not $sync.")
  override def desiredName =
    s"SyncResetSynchronizerShiftReg_w${w}_d${sync}_i$init"
  val output: Seq[Bool] = Seq.tabulate(w) { i =>
    val initBit = ((init >> i) & 1) > 0
    withReset(reset.asBool) {
      SynchronizerPrimitiveShiftReg(
        io.d(i),
        sync,
        initBit,
        SynchronizerResetType.Sync
      )
    }
  }
  io.q := Cat(output.reverse)
}

object SyncResetSynchronizerShiftReg {
  def apply[T <: Chisel.Data](
    in:   T,
    sync: Int,
    init: Int,
    name: Option[String] = None
  ): T =
    if (sync == 0) in
    else
      AbstractPipelineReg(
        new SyncResetSynchronizerShiftReg(in.getWidth, sync, init),
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
