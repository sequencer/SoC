package org.chipsalliance.utils.crossing

import chisel3._

// Note: this should not be used directly.
// Use the companion object to generate this with the correct reset type mixin.
private class SynchronizerPrimitiveShiftReg(
  sync:      Int,
  init:      Boolean,
  resetType: SynchronizerResetType.Value)
    extends AbstractPipelineReg(1) {

  val initInt: Int = if (init) 1 else 0
  val initPostfix: String = resetType match {
    case SynchronizerResetType.NonSync => ""
    case _                             => s"_i$initInt"
  }
  override def desiredName =
    s"${resetType.toString}ResetSynchronizerPrimitiveShiftReg_d$sync$initPostfix"

  val chain: Seq[Bool] = List.tabulate(sync) { i =>
    val reg =
      if (resetType == SynchronizerResetType.NonSync) Reg(Bool())
      else RegInit(init.B)
    reg.suggestName(s"sync_$i")
  }
  chain.last := io.d.asBool

  chain.init.zip(chain.tail).foreach {
    case (sink, source) =>
      sink := source
  }
  io.q := chain.head.asUInt
}

private object SynchronizerPrimitiveShiftReg {
  def apply(
    in:        Bool,
    sync:      Int,
    init:      Boolean,
    resetType: SynchronizerResetType.Value
  ): Bool = {
    val gen: () => SynchronizerPrimitiveShiftReg = resetType match {
      case SynchronizerResetType.NonSync =>
        () => new SynchronizerPrimitiveShiftReg(sync, init, resetType)
      case SynchronizerResetType.Async =>
        () => new SynchronizerPrimitiveShiftReg(sync, init, resetType) with RequireAsyncReset
      case SynchronizerResetType.Sync =>
        () => new SynchronizerPrimitiveShiftReg(sync, init, resetType) with RequireSyncReset
      case SynchronizerResetType.Inferred =>
        () => new SynchronizerPrimitiveShiftReg(sync, init, resetType)
    }
    AbstractPipelineReg(gen(), in)
  }
}
