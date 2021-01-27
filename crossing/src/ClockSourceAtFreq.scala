package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.experimental.DoubleParam
import chisel3.util.HasBlackBoxInline

/** This clock source is only intended to be used in test harnesses, and does not work correctly in verilator. */
class ClockSourceAtFreq(val freqMHz: Double)
    extends BlackBox(
      Map(
        "PERIOD_PS" -> DoubleParam(1000000 / freqMHz)
      )
    )
    with HasBlackBoxInline {
  val io: Record = IO(new ClockSourceIO)

  setInline(
    "ClockSourceAtFreq.v",
    s"""
       |module ClockSourceAtFreq #(parameter PERIOD_PS="") (
       |    input power,
       |    input gate,
       |    output clk);
       |  timeunit 1ps/1ps;
       |
       |  reg clk_i;
       |  initial
       |    clk_i = 1'b0;
       |  always
       |    clk_i = #(PERIOD_PS/2.0) ~clk_i & (power & ~gate);
       |  assign
       |    clk = clk_i;
       |endmodule
       |""".stripMargin
  )
}
