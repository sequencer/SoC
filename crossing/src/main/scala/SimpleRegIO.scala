package org.chipsalliance.utils.crossing

import chisel3._

class SimpleRegIO(val w: Int) extends Bundle {
  val d = Input(UInt(w.W))
  val q = Output(UInt(w.W))
  val en = Input(Bool())
}
