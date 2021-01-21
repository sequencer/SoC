package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.{withClockAndReset, withReset}

/** Reset: asynchronous assert,
  * synchronous de-assert
  *
  * Jiuyang:
  * PSD removed, since it is no related to crossing, if needed, I think it should added with Aspect.
  */

class ResetCatchAndSync(sync: Int = 3) extends Module {

  override def desiredName = s"ResetCatchAndSync_d${sync}"

  class ResetCatchAndSyncBundle extends Bundle {
    val sync_reset = Output(Bool())
  }
  val io = IO(new ResetCatchAndSyncBundle)

  // implemented by Jiuyang
  io.sync_reset := withClockAndReset(clock, reset) { RegNext(RegNext(true.B, false.B), false.B).asAsyncReset }
}

object ResetCatchAndSync {

  def apply(clk: Clock, rst: Bool, sync: Int = 3, name: Option[String] = None): Bool = {
    withClockAndReset(clk, rst) {
      val catcher = Module(new ResetCatchAndSync(sync))
      if (name.isDefined) {
        catcher.suggestName(name.get)
      }
      catcher.io.sync_reset
    }
  }

  def apply(clk: Clock, rst: Bool, sync: Int, name: String): Bool = apply(clk, rst, sync, Some(name))

  def apply(clk: Clock, rst: Bool, name: String): Bool = apply(clk, rst, name = Some(name))
}
