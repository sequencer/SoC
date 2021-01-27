package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

class CrossingIO[T <: Data](gen: T) extends Bundle {
  // Enqueue clock domain
  val enq_clock: Clock = Input(Clock())
  val enq_reset: Bool = Input(Bool()) // synchronously deasserted wrt. enq_clock
  val enq:       DecoupledIO[T] = Flipped(Decoupled(gen))
  // Dequeue clock domain
  val deq_clock: Clock = Input(Clock())
  val deq_reset: Bool = Input(Bool()) // synchronously deasserted wrt. deq_clock
  val deq:       DecoupledIO[T] = Decoupled(gen)
}
