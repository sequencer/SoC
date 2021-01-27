package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{Cat, DecoupledIO, RegEnable}

class RationalCrossingSource[T <: Data](
  gen:       T,
  direction: RationalDirection = Symmetric)
    extends Module {
  class RationalCrossingSourceBundle extends Bundle {
    val enq: DecoupledIO[T] = Flipped(DecoupledIO(gen))
    val deq: RationalIO[T] = RationalIO(gen)
  }
  val io: RationalCrossingSourceBundle = IO(new RationalCrossingSourceBundle)

  val enq_in: DecoupledIO[T] = BlockDuringReset(io.enq)
  val deq:    RationalIO[T] = io.deq
  val enq: DecoupledIO[T] = direction match {
    case Symmetric  => ShiftQueue(enq_in, 1, flow = true)
    case Flexible   => ShiftQueue(enq_in)
    case FastToSlow => enq_in
    case SlowToFast => ShiftQueue(enq_in)
  }

  val count: UInt = RegInit(0.U(2.W))
  val equal: Bool = count === deq.sink

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
