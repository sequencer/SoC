package org.chipsalliance.utils.crossing

import chisel3._

class AsyncBundle[T <: Data](
  private val gen: T,
  val params:      AsyncQueueParams = AsyncQueueParams())
    extends Bundle {
  // Data-path synchronization
  val mem = Output(Vec(params.wires, gen))
  val ridx = Input(UInt((params.bits + 1).W))
  val widx = Output(UInt((params.bits + 1).W))
  val index = params.narrow.option(Input(UInt(params.bits.W)))

  // Signals used to self-stabilize a safe AsyncQueue
  val safe = params.safe.option(new AsyncBundleSafety)
}
