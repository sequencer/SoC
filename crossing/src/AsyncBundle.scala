package org.chipsalliance.utils.crossing

import chisel3._

class AsyncBundle[T <: Data](
  private val gen: T,
  val params:      AsyncQueueParams = AsyncQueueParams())
    extends Bundle {
  // Data-path synchronization
  val mem:   Vec[T] = Output(Vec(params.wires, gen))
  val ridx:  UInt = Input(UInt((params.bits + 1).W))
  val widx:  UInt = Output(UInt((params.bits + 1).W))
  val index: Option[UInt] = if (params.narrow) Some(Input(UInt(params.bits.W))) else None

  // Signals used to self-stabilize a safe AsyncQueue
  val safe: Option[AsyncBundleSafety] = if (params.safe) Some(new AsyncBundleSafety) else None
}
