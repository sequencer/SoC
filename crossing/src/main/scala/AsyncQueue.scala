package org.chipsalliance.utils.crossing

import chisel3._

class AsyncQueue[T <: Data](
  gen:    T,
  params: AsyncQueueParams = AsyncQueueParams())
    extends Module {
  val io = IO(new CrossingIO(gen))
  val source = Module(new AsyncQueueSource(gen, params))
  val sink = Module(new AsyncQueueSink(gen, params))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink.clock := io.deq_clock
  sink.reset := io.deq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
  sink.io.async <> source.io.async
}
