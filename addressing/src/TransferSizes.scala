package org.chipsalliance.utils.addressing

import chisel3._
import chisel3.util.{isPow2, log2Ceil}

import scala.language.implicitConversions

// An potentially empty inclusive range of 2-powers [min, max] (in bytes)
case class TransferSizes(min: Int, max: Int) {
  def this(x: Int) = this(x, x)

  require(min <= max, s"Min transfer $min > max transfer $max")
  require(min >= 0 && max >= 0, s"TransferSizes must be positive, got: ($min, $max)")
  require(max == 0 || isPow2(max), s"TransferSizes must be a power of 2, got: $max")
  require(min == 0 || isPow2(min), s"TransferSizes must be a power of 2, got: $min")
  require(max == 0 || min != 0, s"TransferSize 0 is forbidden unless (0,0), got: ($min, $max)")

  def none: Boolean = min == 0
  def contains(x:   Int): Boolean = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int): Boolean = contains(1 << x)
  def containsLg(x: UInt): Bool =
    if (none) false.B
    else if (min == max) { log2Ceil(min).U === x }
    else log2Ceil(min).U <= x && x <= log2Ceil(max).U

  def contains(x: TransferSizes): Boolean = x.none || (min <= x.min && x.max <= max)

  def intersect(x: TransferSizes): TransferSizes =
    if (x.max < min || max < x.min) TransferSizes.none
    else TransferSizes(scala.math.max(min, x.min), scala.math.min(max, x.max))

  // Not a union, because the result may contain sizes contained by neither term
  // NOT TO BE CONFUSED WITH COVERPOINTS
  def mincover(x: TransferSizes): TransferSizes = {
    if (none) {
      x
    } else if (x.none) {
      this
    } else {
      TransferSizes(scala.math.min(min, x.min), scala.math.max(max, x.max))
    }
  }

  override def toString: String = "TransferSizes[%d, %d]".format(min, max)
}

object TransferSizes {
  def apply(x: Int) = new TransferSizes(x)
  val none = new TransferSizes(0)

  def mincover(seq:  Seq[TransferSizes]): TransferSizes = seq.foldLeft(none)(_ mincover _)
  def intersect(seq: Seq[TransferSizes]): TransferSizes = seq.reduce(_ intersect _)

  implicit def asBool(x: TransferSizes): Boolean = !x.none
}
