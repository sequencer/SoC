package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

class AsyncQueueSource[T <: Data](
  gen:    T,
  params: AsyncQueueParams = AsyncQueueParams())
    extends Module {
  class AsyncQueueSourceBundle extends Bundle {
    // These come from the source domain
    val enq: DecoupledIO[T] = Flipped(Decoupled(gen))
    // These cross to the sink clock domain
    val async = new AsyncBundle(gen, params)
  }
  val io:   AsyncQueueSourceBundle = IO(new AsyncQueueSourceBundle)
  val bits: Int = params.bits
  val sink_ready = WireInit(true.B)
  val mem: Vec[T] = Reg(Vec(params.depth, gen)) // This does NOT need to be reset at all.
  val widx: UInt = withReset(reset.asAsyncReset)(
    GrayCounter(bits + 1, io.enq.fire(), !sink_ready, "widx_bin")
  )
  val ridx: UInt = AsyncResetSynchronizerShiftReg(
    io.async.ridx,
    params.sync,
    Some("ridx_gray")
  )
  val ready: Bool = sink_ready && widx =/= (ridx ^ (params.depth | params.depth >> 1).U)

  val index: UInt =
    if (bits == 0) 0.U
    else io.async.widx(bits - 1, 0) ^ (io.async.widx(bits, bits) << (bits - 1))
  when(io.enq.fire()) { mem(index) := io.enq.bits }

  val ready_reg: Bool = withReset(reset.asAsyncReset)(
    RegNext(next = ready, init = false.B).suggestName("ready_reg")
  )
  io.enq.ready := ready_reg && sink_ready

  val widx_reg: UInt = withReset(reset.asAsyncReset)(
    RegNext(next = widx, init = 0.U).suggestName("widx_gray")
  )
  io.async.widx := widx_reg

  io.async.index match {
    case Some(index) => io.async.mem(0) := mem(index)
    case None        => io.async.mem := mem
  }

  io.async.safe.foreach { sio =>
    val source_valid_0 =
      Module(new AsyncValidSync(params.sync, "source_valid_0"))
    val source_valid_1 =
      Module(new AsyncValidSync(params.sync, "source_valid_1"))

    val sink_extend = Module(new AsyncValidSync(params.sync, "sink_extend"))
    val sink_valid = Module(new AsyncValidSync(params.sync, "sink_valid"))
    source_valid_0.reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    source_valid_1.reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    sink_extend.reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    sink_valid.reset := reset.asAsyncReset

    source_valid_0.clock := clock
    source_valid_1.clock := clock
    sink_extend.clock := clock
    sink_valid.clock := clock

    source_valid_0.io.in := true.B
    source_valid_1.io.in := source_valid_0.io.out
    sio.widx_valid := source_valid_1.io.out
    sink_extend.io.in := sio.ridx_valid
    sink_valid.io.in := sink_extend.io.out
    sink_ready := sink_valid.io.out
    sio.source_reset_n := !reset.asBool

  // Assert that if there is stuff in the queue, then reset cannot happen
  //  Impossible to write because dequeue can occur on the receiving side,
  //  then reset allowed to happen, but write side cannot know that dequeue
  //  occurred.
  // TODO: write some sort of sanity check assertion for users
  // that denote don't reset when there is activity
  //    assert (!(reset || !sio.sink_reset_n) || !io.enq.valid, "Enqueue while sink is reset and AsyncQueueSource is unprotected")
  //    assert (!reset_rise || prev_idx_match.asBool, "Sink reset while AsyncQueueSource not empty")
  }
}
