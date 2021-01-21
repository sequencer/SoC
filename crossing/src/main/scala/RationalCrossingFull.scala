package org.chipsalliance.utils.crossing

import chisel3.{Data, Module}

class RationalCrossingFull[T <: Data](
  gen:       T,
  direction: RationalDirection = Symmetric)
    extends Module {
  val io = IO(new CrossingIO(gen))

  val source = Module(new RationalCrossingSource(gen, direction))
  val sink = Module(new RationalCrossingSink(gen, direction))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink.clock := io.deq_clock
  sink.reset := io.deq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
}
