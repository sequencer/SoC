package org.chipsalliance.utils.crossing

import chisel3.util.Cat

class SynchronizerShiftReg(w: Int = 1, sync: Int = 3) extends AbstractPipelineReg(w) {
  require(sync > 1, s"Sync must be greater than 1, not ${sync}.")
  override def desiredName = s"SynchronizerShiftReg_w${w}_d${sync}"
  val output = Seq.tabulate(w) { i =>
    SynchronizerPrimitiveShiftReg(
      io.d(i),
      sync,
      false,
      SynchronizerResetType.NonSync
    )
  }
  io.q := Cat(output.reverse)
}

object SynchronizerShiftReg {
  def apply[T <: Chisel.Data](
    in:   T,
    sync: Int,
    name: Option[String] = None
  ): T =
    if (sync == 0) in
    else
      AbstractPipelineReg(new SynchronizerShiftReg(in.getWidth, sync), in, name)

  def apply[T <: Chisel.Data](in: T, sync: Int): T =
    apply(in, sync, None)

  def apply[T <: Chisel.Data](in: T): T =
    apply(in, 3, None)

}
