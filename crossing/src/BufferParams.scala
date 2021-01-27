package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.{DecoupledIO, Queue, ReadyValidIO}

import scala.language.implicitConversions

case class BufferParams(depth: Int, flow: Boolean, pipe: Boolean) {
  require(depth >= 0, "Buffer depth must be >= 0")
  def isDefined: Boolean = depth > 0
  def latency:   Int = if (isDefined && !flow) 1 else 0

  def apply[T <: Data](x: DecoupledIO[T]): DecoupledIO[T] =
    if (isDefined) Queue(x, depth, flow = flow, pipe = pipe)
    else x

  def irrevocable[T <: Data](x: ReadyValidIO[T]): ReadyValidIO[T] =
    if (isDefined) Queue.irrevocable(x, depth, flow = flow, pipe = pipe)
    else x

  def sq[T <: Data](x: DecoupledIO[T]): DecoupledIO[T] =
    if (!isDefined) x
    else {
      val sq = Module(new ShiftQueue(x.bits, depth, flow = flow, pipe = pipe))
      sq.io.enq <> x
      sq.io.deq
    }

  override def toString: String = "BufferParams:%d%s%s".format(depth, if (flow) "F" else "", if (pipe) "P" else "")

}

object BufferParams {
  implicit def apply(depth: Int): BufferParams = BufferParams(depth, flow = false, pipe = false)

  val default: BufferParams = BufferParams(2)
  val none:    BufferParams = BufferParams(0)
  val flow:    BufferParams = BufferParams(1, flow = true, pipe = false)
  val pipe:    BufferParams = BufferParams(1, flow = false, pipe = true)
}
