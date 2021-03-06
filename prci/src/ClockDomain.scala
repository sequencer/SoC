// See LICENSE.SiFive for license details.

package org.chipsalliance.utils.prci

import chisel3._
import diplomacy._
import org.chipsalliance.utils.crossing.{ClockCrossingType, CrossingType}

trait HasDomainCrossing extends LazyScope { this: LazyModule =>
  type DomainCrossingType <: CrossingType
}

trait HasClockDomainCrossing extends HasDomainCrossing { this: LazyModule =>
  type DomainCrossingType = ClockCrossingType
}

abstract class Domain extends LazyModule with HasDomainCrossing {
  def clockBundle: ClockBundle

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    childClock := clockBundle.clock
    childReset := clockBundle.reset

    // these are just for backwards compatibility with external devices
    // that were manually wiring themselves to the domain's clock/reset input:
    val clock: Clock = IO(Output(chiselTypeOf(clockBundle.clock)))
    val reset: Reset = IO(Output(chiselTypeOf(clockBundle.reset)))
    clock := clockBundle.clock
    reset := clockBundle.reset
  }
}

abstract class ClockDomain extends Domain with HasClockDomainCrossing

class ClockSinkDomain(val clockSinkParams: ClockSinkParameters) extends ClockDomain {
  def this(take: Option[ClockParameters] = None, name: Option[String] = None) =
    this(ClockSinkParameters(take = take, name = name))
  val clockNode:   ClockSinkNode = ClockSinkNode(Seq(clockSinkParams))
  def clockBundle: ClockBundle = clockNode.in.head._1
}

class ClockSourceDomain(val clockSourceParams: ClockSourceParameters) extends ClockDomain {
  def this(give: Option[ClockParameters] = None, name: Option[String] = None) =
    this(ClockSourceParameters(give = give, name = name))
  val clockNode:   ClockSourceNode = ClockSourceNode(Seq(clockSourceParams))
  def clockBundle: ClockBundle = clockNode.out.head._1
}

abstract class ResetDomain extends Domain with HasResetDomainCrossing
