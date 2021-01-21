package org.chipsalliance.utils.crossing

import chisel3._

class ClockSourceIO extends Bundle {
  val power = Input(Bool())
  val gate = Input(Bool())
  val clk = Output(Clock())
}
