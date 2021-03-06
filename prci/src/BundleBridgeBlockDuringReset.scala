// See LICENSE.SiFive for license details.

package org.chipsalliance.utils.prci

import chisel3._
import diplomacy._
import diplomacy.BundleBridgeNexus.fillN
import org.chipsalliance.utils.crossing.{BlockDuringReset, Blockable}

object BundleBridgeBlockDuringReset {
  def apply[T <: Data: Blockable](
    resetCrossingType:   ResetCrossingType,
    name:                Option[String] = None,
    registered:          Boolean = false,
    default:             Option[() => T] = None,
    inputRequiresOutput: Boolean = false,
    shouldBeInlined:     Boolean = true
  ): BundleBridgeNexusNode[T] = {
    val nexus = LazyModule(
      new BundleBridgeNexus[T](
        inputFn = (s: Seq[T]) => {
          val data = BundleBridgeNexus.requireOne[T](registered)(s)
          resetCrossingType match {
            case _: NoResetCrossing        => data
            case s: StretchedResetCrossing => BlockDuringReset(data, s.cycles)
          }
        },
        outputFn = fillN[T](registered),
        default = default,
        inputRequiresOutput = inputRequiresOutput,
        shouldBeInlined = shouldBeInlined
      )
    )
    name.foreach(nexus.suggestName)
    nexus.node
  }
}
