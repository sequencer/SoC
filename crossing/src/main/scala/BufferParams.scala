package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{DecoupledIO, Queue, ReadyValidIO}

case class BufferParams(depth: Int, flow: Boolean, pipe: Boolean) {
  require(depth >= 0, "Buffer depth must be >= 0")
  def isDefined = depth > 0
  def latency = if (isDefined && !flow) 1 else 0

  def apply[T <: Data](x: DecoupledIO[T]) =
    if (isDefined) Queue(x, depth, flow = flow, pipe = pipe)
    else x

  def irrevocable[T <: Data](x: ReadyValidIO[T]) =
    if (isDefined) Queue.irrevocable(x, depth, flow = flow, pipe = pipe)
    else x

  def sq[T <: Data](x: DecoupledIO[T]) =
    if (!isDefined) x
    else {
      val sq = Module(new ShiftQueue(x.bits, depth, flow = flow, pipe = pipe))
      sq.io.enq <> x
      sq.io.deq
    }

  override def toString() = "BufferParams:%d%s%s".format(depth, if (flow) "F" else "", if (pipe) "P" else "")

}

object BufferParams {
  implicit def apply(depth: Int): BufferParams = BufferParams(depth, false, false)

  val default = BufferParams(2)
  val none = BufferParams(0)
  val flow = BufferParams(1, true, false)
  val pipe = BufferParams(1, false, true)
}
