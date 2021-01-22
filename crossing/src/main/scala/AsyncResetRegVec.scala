package org.chipsalliance.utils.crossing

import chisel3._

class AsyncResetRegVec(val w: Int, val init: BigInt) extends Module {
  override def desiredName = s"AsyncResetRegVec_w${w}_i$init"

  val io: SimpleRegIO = IO(new SimpleRegIO(w))

  val reg: UInt = withReset(reset.asAsyncReset)(RegInit(init.U(w.W)))
  when(io.en) {
    reg := io.d
  }
  io.q := reg
}
