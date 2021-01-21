package org.chipsalliance.utils.crossing

case class AsynchronousCrossing(
  depth:      Int = 8,
  sourceSync: Int = 3,
  sinkSync:   Int = 3,
  safe:       Boolean = true,
  narrow:     Boolean = false)
    extends ClockCrossingType {
  def asSinkParams = AsyncQueueParams(depth, sinkSync, safe, narrow)
}
