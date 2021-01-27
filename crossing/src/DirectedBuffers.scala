package org.chipsalliance.utils.crossing

trait DirectedBuffers[T] {
  def copyIn(x:    BufferParams): T
  def copyOut(x:   BufferParams): T
  def copyInOut(x: BufferParams): T
}
