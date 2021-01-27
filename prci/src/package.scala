// See LICENSE.SiFive for license details.

package org.chipsalliance.utils

import diplomacy.{InwardNodeHandle, NodeHandle, OutwardNodeHandle}
import org.chipsalliance.utils.crossing.{AsynchronousCrossing, ClockCrossingType}

package object prci {
  type ClockInwardNode = InwardNodeHandle[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
  type ClockOutwardNode =
    OutwardNodeHandle[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
  type ClockNode = NodeHandle[
    ClockSourceParameters,
    ClockSinkParameters,
    ClockEdgeParameters,
    ClockBundle,
    ClockSourceParameters,
    ClockSinkParameters,
    ClockEdgeParameters,
    ClockBundle
  ]
  def asyncMux[T](xType: ClockCrossingType, async: T, notasync: T): T = xType match {
    case _: AsynchronousCrossing => async
    case _ => notasync
  }
}
