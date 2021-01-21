package org.chipsalliance.utils.addressing

import chisel3._
import chisel3.util.log2Floor

// A non-empty half-open range; [start, end)
case class IdRange(start: Int, end: Int) extends Ordered[IdRange] {
  require(start >= 0, s"Ids cannot be negative, but got: $start.")
  require(start <= end, "Id ranges cannot be negative.")

  def compare(x: IdRange) = {
    val primary = (this.start - x.start).signum
    val secondary = (x.end - this.end).signum
    if (primary != 0) primary else secondary
  }

  def overlaps(x: IdRange) = start < x.end && x.start < end
  def contains(x: IdRange) = start <= x.start && x.end <= end

  def contains(x: Int) = start <= x && x < end
  def contains(x: UInt) =
    if (size == 0) {
      false.B
    } else if (size == 1) { // simple comparison
      x === start.U
    } else {
      // find index of largest different bit
      val largestDeltaBit = log2Floor(start ^ (end - 1))
      val smallestCommonBit = largestDeltaBit + 1 // may not exist in x
      val uncommonMask = (1 << smallestCommonBit) - 1
      val uncommonBits = (x | 0.U(smallestCommonBit.W))(largestDeltaBit, 0)
      // the prefix must match exactly (note: may shift ALL bits away)
      (x >> smallestCommonBit) === (start >> smallestCommonBit).U &&
      // firrtl constant prop range analysis can eliminate these two:
      (start & uncommonMask).U <= uncommonBits &&
      uncommonBits <= ((end - 1) & uncommonMask).U
    }

  def shift(x: Int) = IdRange(start + x, end + x)
  def size = end - start
  def isEmpty = end == start

  def range = start until end
}

object IdRange {
  def overlaps(s: Seq[IdRange]) = if (s.isEmpty) None
  else {
    val ranges = s.sorted
    (ranges.tail.zip(ranges.init)).find { case (a, b) => a.overlaps(b) }
  }
}
