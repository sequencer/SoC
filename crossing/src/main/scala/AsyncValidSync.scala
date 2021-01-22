package org.chipsalliance.utils.crossing

import chisel3._

class AsyncValidSync(sync: Int, desc: String) extends RawModule {
  class AsyncValidSync extends Bundle {
    val in:  Bool = Input(Bool())
    val out: Bool = Output(Bool())
  }
  val io:    AsyncValidSync = IO(new AsyncValidSync)
  val clock: Clock = IO(Input(Clock()))
  val reset: AsyncReset = IO(Input(AsyncReset()))
  withClockAndReset(clock, reset) {
    io.out := AsyncResetSynchronizerShiftReg(io.in, sync, Some(desc))
  }
}
