package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.HasBlackBoxInline

/** This black-boxes a Clock Divider by 2.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  */
class ClockDivider2 extends BlackBox with HasBlackBoxInline {
  class ClockDivider2Bundle extends Bundle {
    val clk_out: Clock = Output(Clock())
    val clk_in:  Clock = Input(Clock())
  }
  val io: ClockDivider2Bundle = IO(new ClockDivider2Bundle)

  setInline(
    "ClockDivider2",
    s"""module ClockDivider2 (
       |  output reg clk_out,
       |  input clk_in
       |);
       | initial clk_out = 1'b0;
       | always @(posedge clk_in) begin
       |    clk_out = ~clk_out;
       | end
       |endmodule // ClockDivider2
       |""".stripMargin
  )
}
