// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import diplomacy.LazyModule
import org.chipsalliance.utils.crossing.CrossingType

trait HasResetDomainCrossing extends HasDomainCrossing { this: LazyModule =>
  type DomainCrossingType = ResetCrossingType
}

sealed trait ResetCrossingType extends CrossingType {
  def injectClockNode: ClockNode
}

case class NoResetCrossing() extends ResetCrossingType {
  def injectClockNode: ClockNode = ClockTempNode()
}

case class StretchedResetCrossing(cycles: Int) extends ResetCrossingType {
  def injectClockNode: ClockNode = {
    val rs = LazyModule(new ResetStretcher(cycles))
    rs.node
  }
}
