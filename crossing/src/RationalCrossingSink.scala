package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util._

class RationalCrossingSink[T <: Data](
  gen:       T,
  direction: RationalDirection = Symmetric)
    extends Module {
  class RationalCrossingSinkBundle extends Bundle {
    val enq: RationalIO[T] = Flipped(RationalIO(gen))
    val deq: DecoupledIO[T] = Decoupled(gen)
  }
  val io: RationalCrossingSinkBundle = IO(new RationalCrossingSinkBundle)

  val enq: RationalIO[T] = io.enq
  val deq: DecoupledIO[T] = Wire(chiselTypeOf(io.deq))
  direction match {
    case Symmetric  => io.deq <> ShiftQueue(deq, 1, pipe = true)
    case Flexible   => io.deq <> ShiftQueue(deq)
    case FastToSlow => io.deq <> ShiftQueue(deq)
    case SlowToFast => io.deq <> deq
  }

  val count: UInt = RegInit(0.U(2.W))
  val equal: Bool = count === enq.source

  enq.ready := deq.ready
  enq.sink := count
  deq.bits := Mux(equal, enq.bits0, enq.bits1)
  deq.valid := Mux(equal, enq.valid, count(1) =/= enq.source(0))

  when(deq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
    case Flexible   => ()
    case FastToSlow => assert(equal || count(1) =/= enq.source(0))
    case SlowToFast => assert(equal || count(1) === enq.source(0))
  }
}
