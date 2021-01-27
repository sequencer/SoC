package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.HasBlackBoxInline

class ClockDivider3 extends BlackBox with HasBlackBoxInline {
  class ClockDivider3Bundle extends Bundle {
    val clk_out: Clock = Output(Clock())
    val clk_in:  Clock = Input(Clock())
  }
  val io: ClockDivider3Bundle = IO(new ClockDivider3Bundle)

  setInline(
    "ClockDivider3",
    s"""module ClockDivider3 (
       |  output reg clk_out,
       |  input clk_in
       |);
       |  reg delay;
       |  initial begin
       |    clk_out = 1'b0;
       |    delay = 1'b0;
       |  end
       |  always @(posedge clk_in) begin
       |     if (clk_out == 1'b0) begin
       |       clk_out = 1'b1;
       |       delay <= 1'b0;
       |     end else if (delay == 1'b1) begin
       |       clk_out = 1'b0;
       |       delay <= 1'b0;
       |     end else begin
       |       delay <= 1'b1;
       |     end
       |  end
       |endmodule // ClockDivider3
       |""".stripMargin
  )
}
