package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

class AsyncQueueSink[T <: Data](
  gen:    T,
  params: AsyncQueueParams = AsyncQueueParams())
    extends Module {
  class AsyncQueueSinkBundle extends Bundle {
    // These come from the sink domain
    val deq: DecoupledIO[T] = Decoupled(gen)
    // These cross to the source clock domain
    val async: AsyncBundle[T] = Flipped(new AsyncBundle(gen, params))
  }
  val io: AsyncQueueSinkBundle = IO(new AsyncQueueSinkBundle)

  val bits: Int = params.bits
  val source_ready = WireInit(true.B)
  val ridx: UInt = withReset(reset.asAsyncReset)(
    GrayCounter(bits + 1, io.deq.fire(), !source_ready, "ridx_bin")
  )
  val widx: UInt = AsyncResetSynchronizerShiftReg(
    io.async.widx,
    params.sync,
    Some("widx_gray")
  )
  val valid: Bool = source_ready && ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index: UInt =
    if (bits == 0) 0.U else ridx(bits - 1, 0) ^ (ridx(bits, bits) << (bits - 1))
  io.async.index.foreach { _ := index }
  // This register does not NEED to be reset, as its contents will not
  // be considered unless the asynchronously reset deq valid register is set.
  // It is possible that bits latches when the source domain is reset / has power cut
  // This is safe, because isolation gates brought mem low before the zeroed widx reached us
  val deq_bits_nxt: T = io.async.mem(if (params.narrow) 0.U else index)
  io.deq.bits := ClockCrossingReg(
    deq_bits_nxt,
    en = valid,
    doInit = false,
    name = Some("deq_bits_reg")
  )

  val valid_reg: Bool = withReset(reset.asAsyncReset)(
    RegNext(next = valid, init = false.B).suggestName("valid_reg")
  )
  io.deq.valid := valid_reg && source_ready

  val ridx_reg: UInt = withReset(reset.asAsyncReset)(
    RegNext(next = ridx, init = 0.U).suggestName("ridx_gray")
  )
  io.async.ridx := ridx_reg

  io.async.safe.foreach { sio =>
    val sink_valid_0 = Module(new AsyncValidSync(params.sync, "sink_valid_0"))
    val sink_valid_1 = Module(new AsyncValidSync(params.sync, "sink_valid_1"))

    val source_extend = Module(new AsyncValidSync(params.sync, "source_extend"))
    val source_valid = Module(new AsyncValidSync(params.sync, "source_valid"))
    sink_valid_0.reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    sink_valid_1.reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    source_extend.reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    source_valid.reset := reset.asAsyncReset

    sink_valid_0.clock := clock
    sink_valid_1.clock := clock
    source_extend.clock := clock
    source_valid.clock := clock

    sink_valid_0.io.in := true.B
    sink_valid_1.io.in := sink_valid_0.io.out
    sio.ridx_valid := sink_valid_1.io.out
    source_extend.io.in := sio.widx_valid
    source_valid.io.in := source_extend.io.out
    source_ready := source_valid.io.out
    sio.sink_reset_n := !reset.asBool

  // TODO: write some sort of sanity check assertion for users
  // that denote don't reset when there is activity
  //
  // val reset_and_extend = !source_ready || !sio.source_reset_n || reset.asBool
  // val reset_and_extend_prev = RegNext(reset_and_extend, true.B)
  // val reset_rise = !reset_and_extend_prev && reset_and_extend
  // val prev_idx_match = AsyncResetReg(updateData=(io.async.widx===io.async.ridx), resetData=0)
  // assert (!reset_rise || prev_idx_match.asBool, "Source reset while AsyncQueueSink not empty")
  }
}
