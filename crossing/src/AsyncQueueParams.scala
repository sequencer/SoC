package org.chipsalliance.utils.crossing

import chisel3.util.{isPow2, log2Ceil}

case class AsyncQueueParams(
  depth: Int = 8,
  sync:  Int = 3,
  safe:  Boolean = true,
  // If safe is true, then effort is made to resynchronize the crossing indices when either side is reset.
  // This makes it safe/possible to reset one side of the crossing (but not the other) when the queue is empty.
  narrow: Boolean = false)
// If narrow is true then the read mux is moved to the source side of the crossing.
// This reduces the number of level shifters in the case where the clock crossing is also a voltage crossing,
// at the expense of a combinational path from the sink to the source and back to the sink.
{
  require(depth > 0 && isPow2(depth))
  require(sync >= 2)

  val bits:  Int = log2Ceil(depth)
  val wires: Int = if (narrow) 1 else depth
}

object AsyncQueueParams {
  // When there is only one entry, we don't need narrow.
  def singleton(sync: Int = 3, safe: Boolean = true): AsyncQueueParams =
    AsyncQueueParams(1, sync, safe)
}
