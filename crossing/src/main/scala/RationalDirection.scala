package org.chipsalliance.utils.crossing

// If you know two clocks are related with an N:M relationship, you
// can cross the clock domains with lower latency than an AsyncQueue.
// This crossing adds 1 cycle in the target clock domain.

// A rational crossing must put registers on the slow side.
// This trait covers the options of how/where to put the registers.
// BEWARE: the source+sink must agree on the direction!
sealed trait RationalDirection {
  def flip: RationalDirection
}

// If it's unclear which side will be slow (or it is variable),
// place registers on both sides of the crossing, by splitting
// a Queue into flow and pipe parts on either side. This is safe
// for all possible clock ratios, but has the downside that the
// timing must be met for the least-common-multiple of the clocks.
case object Symmetric extends RationalDirection {
  def flip = Symmetric
}

// Like Symmetric, this crossing works for all ratios N:M.
// However, unlike the other crossing options, this varient adds
// a full flow+pipe buffer on both sides of the crossing. This
// ends up costing potentially two cycles of delay, but gives
// both clock domains a full clock period to close timing.
case object Flexible extends RationalDirection {
  def flip = Flexible
}

// If the source is N:1 of the sink, place the registers at the sink.
// This imposes only a single clock cycle of delay and both side of
// the crossing have a full clock period to close timing.
case object FastToSlow extends RationalDirection {
  def flip = SlowToFast
}

// If the source is 1:N of the sink, place the registers at the source.
// This imposes only a single clock cycle of delay and both side of
// the crossing have a full clock period to close timing.
case object SlowToFast extends RationalDirection {
  def flip = FastToSlow
}
