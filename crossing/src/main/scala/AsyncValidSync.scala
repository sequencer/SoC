package org.chipsalliance.utils.crossing

import chisel3._

class AsyncValidSync(sync: Int, desc: String) extends RawModule {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })
  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  withClockAndReset(clock, reset) {
    io.out := AsyncResetSynchronizerShiftReg(io.in, sync, Some(desc))
  }
}
