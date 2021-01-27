package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.HasBlackBoxInline

/** This clock source is only intended to be used in test harnesses, and does not work correctly in verilator. */
class ClockSourceAtFreqFromPlusArg(val plusArgName: String) extends BlackBox with HasBlackBoxInline {
  val io: Record = IO(new ClockSourceIO)

  override def desiredName = s"ClockSourceAtFreqFromPlusArg$plusArgName"

  setInline(
    s"$desiredName.v",
    s"""
       |module $desiredName (
       |    input power,
       |    input gate,
       |    output clk);
       |  timeunit 1ps/1ps;
       |
       |  reg clk_i;
       |  real FREQ_MHZ;
       |  real PERIOD_PS;
       |  initial begin
       |    clk_i = 1'b0;
       |    if (!$$value$$plusargs("$plusArgName=%d", FREQ_MHZ)) begin
       |      FREQ_MHZ = 100.0;
       |    end
       |    PERIOD_PS = 1000000.0 / FREQ_MHZ;
       |    forever #(PERIOD_PS/2.0) clk_i = ~clk_i & (power & ~gate);
       |  end
       |  assign clk = clk_i;
       |endmodule
       |""".stripMargin
  )
}
