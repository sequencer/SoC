package org.chipsalliance.utils.crossing

import chisel3._

class ClockSourceIO extends Bundle {
  val power: Bool = Input(Bool())
  val gate:  Bool = Input(Bool())
  val clk:   Clock = Output(Clock())
}
