package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{Cat, DecoupledIO, RegEnable}

class RationalCrossingSource[T <: Data](
  gen:       T,
  direction: RationalDirection = Symmetric)
    extends Module {
  class RationalCrossingSourceBundle extends Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = RationalIO(gen)
  }
  val io = IO(new RationalCrossingSourceBundle)

  val enq_in = BlockDuringReset(io.enq)
  val deq = io.deq
  val enq = direction match {
    case Symmetric  => ShiftQueue(enq_in, 1, flow = true)
    case Flexible   => ShiftQueue(enq_in, 2)
    case FastToSlow => enq_in
    case SlowToFast => ShiftQueue(enq_in, 2)
  }

  val count = RegInit(0.U(2.W))
  val equal = count === deq.sink

  deq.valid := enq.valid
  deq.source := count
  deq.bits0 := enq.bits
  deq.bits1 := RegEnable(enq.bits, equal)
  enq.ready := Mux(equal, deq.ready, count(1) =/= deq.sink(0))

  when(enq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
    case Flexible   => ()
    case FastToSlow => assert(equal || count(1) === deq.sink(0))
    case SlowToFast => assert(equal || count(1) =/= deq.sink(0))
  }
}
