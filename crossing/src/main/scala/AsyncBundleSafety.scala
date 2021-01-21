package org.chipsalliance.utils.crossing

import chisel3._

class AsyncBundleSafety extends Bundle {
  val ridx_valid = Input(Bool())
  val widx_valid = Output(Bool())
  val source_reset_n = Output(Bool())
  val sink_reset_n = Input(Bool())
}
