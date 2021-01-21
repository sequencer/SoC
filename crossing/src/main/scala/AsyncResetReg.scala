package org.chipsalliance.utils.crossing

import chisel3._

class AsyncResetReg(resetValue: Int = 0) extends RawModule {
  val io = IO(new Bundle {
    val d = Input(Bool())
    val q = Output(Bool())
    val en = Input(Bool())

    val clk = Input(Clock())
    val rst = Input(Bool())
  })

  val reg =
    withClockAndReset(io.clk, io.rst.asAsyncReset)(RegInit(resetValue.U(1.W)))
  when(io.en) {
    reg := io.d
  }
  io.q := reg
}

object AsyncResetReg {
  // Create Single Registers
  def apply(
    d:    Bool,
    clk:  Clock,
    rst:  Bool,
    init: Boolean,
    name: Option[String]
  ): Bool = {
    val reg = Module(new AsyncResetReg(if (init) 1 else 0))
    reg.io.d := d
    reg.io.clk := clk
    reg.io.rst := rst
    reg.io.en := true.B
    name.foreach(reg.suggestName(_))
    reg.io.q
  }

  def apply(d: Bool, clk: Clock, rst: Bool): Bool =
    apply(d, clk, rst, false, None)
  def apply(d: Bool, clk: Clock, rst: Bool, name: String): Bool =
    apply(d, clk, rst, false, Some(name))

  // Create Vectors of Registers
  def apply(
    updateData: UInt,
    resetData:  BigInt,
    enable:     Bool,
    name:       Option[String] = None
  ): UInt = {
    val w = updateData.getWidth.max(resetData.bitLength)
    val reg = Module(new AsyncResetRegVec(w, resetData))
    name.foreach(reg.suggestName(_))
    reg.io.d := updateData
    reg.io.en := enable
    reg.io.q
  }
  def apply(
    updateData: UInt,
    resetData:  BigInt,
    enable:     Bool,
    name:       String
  ): UInt = apply(updateData, resetData, enable, Some(name))

  def apply(updateData: UInt, resetData: BigInt): UInt =
    apply(updateData, resetData, enable = true.B)
  def apply(updateData: UInt, resetData: BigInt, name: String): UInt =
    apply(updateData, resetData, enable = true.B, Some(name))

  def apply(updateData: UInt, enable: Bool): UInt =
    apply(updateData, resetData = BigInt(0), enable)
  def apply(updateData: UInt, enable: Bool, name: String): UInt =
    apply(updateData, resetData = BigInt(0), enable, Some(name))

  def apply(updateData: UInt): UInt =
    apply(updateData, resetData = BigInt(0), enable = true.B)
  def apply(updateData: UInt, name: String): UInt =
    apply(updateData, resetData = BigInt(0), enable = true.B, Some(name))
}
