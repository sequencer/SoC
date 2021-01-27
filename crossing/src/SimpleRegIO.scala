package org.chipsalliance.utils.crossing

import chisel3._

class SimpleRegIO(val w: Int) extends Bundle {
  val d:  UInt = Input(UInt(w.W))
  val q:  UInt = Output(UInt(w.W))
  val en: Bool = Input(Bool())
}
