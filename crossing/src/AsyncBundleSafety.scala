package org.chipsalliance.utils.crossing

import chisel3._

class AsyncBundleSafety extends Bundle {
  val ridx_valid:     Bool = Input(Bool())
  val widx_valid:     Bool = Output(Bool())
  val source_reset_n: Bool = Output(Bool())
  val sink_reset_n:   Bool = Input(Bool())
}
