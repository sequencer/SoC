package org.chipsalliance.utils.crossing

import chisel3._

class AsyncResetShiftReg(
  w:     Int = 1,
  depth: Int = 1,
  init:  Int = 0,
  name:  String = "pipe")
    extends AbstractPipelineReg(w) {
  require(depth > 0, "Depth must be greater than 0.")

  override def desiredName = s"AsyncResetShiftReg_w${w}_d${depth}_i${init}"

  val chain = List.tabulate(depth) { i =>
    Module(new AsyncResetRegVec(w, init)).suggestName(s"${name}_${i}")
  }

  chain.last.io.d := io.d
  chain.last.io.en := true.B

  (chain.init.zip(chain.tail)).foreach {
    case (sink, source) =>
      sink.io.d := source.io.q
      sink.io.en := true.B
  }
  io.q := chain.head.io.q
}

object AsyncResetShiftReg {
  def apply[T <: Chisel.Data](
    in:    T,
    depth: Int,
    init:  Int = 0,
    name:  Option[String] = None
  ): T =
    AbstractPipelineReg(
      new AsyncResetShiftReg(in.getWidth, depth, init),
      in,
      name
    )

  def apply[T <: Chisel.Data](in: T, depth: Int, name: Option[String]): T =
    apply(in, depth, 0, name)

  def apply[T <: Chisel.Data](
    in:    T,
    depth: Int,
    init:  T,
    name:  Option[String]
  ): T =
    apply(in, depth, init.litValue.toInt, name)

  def apply[T <: Chisel.Data](in: T, depth: Int, init: T): T =
    apply(in, depth, init.litValue.toInt, None)
}
