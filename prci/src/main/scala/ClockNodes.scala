// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import org.chipsalliance.utils.dts.FixedClockResource

object ClockImp extends SimpleNodeImp[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle] {
  def edge(
    pd:         ClockSourceParameters,
    pu:         ClockSinkParameters,
    sourceInfo: SourceInfo
  ): ClockEdgeParameters =
    ClockEdgeParameters(pd, pu, sourceInfo)
  def bundle(e: ClockEdgeParameters) = new ClockBundle(e.bundle)
  def render(e: ClockEdgeParameters): RenderedEdge = RenderedEdge(colour = "#00cc00" /* green */ )
}

case class ClockSourceNode(portParams: Seq[ClockSourceParameters])(implicit valName: sourcecode.Name)
    extends SourceNode(ClockImp)(portParams) {
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = portParams.map {
    p =>
      p.give.map(g => new FixedClockResource(name, g.freqMHz, prefix))
  }
}

case class ClockSinkNode(portParams: Seq[ClockSinkParameters])(implicit valName: sourcecode.Name)
    extends SinkNode(ClockImp)(portParams) {
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = portParams.map {
    p =>
      p.take.map(t => new FixedClockResource(name, t.freqMHz, prefix))
  }
}

case class ClockAdapterNode(
  sourceFn: ClockSourceParameters => ClockSourceParameters = { m => m },
  sinkFn:   ClockSinkParameters => ClockSinkParameters = { s => s }
)(
  implicit valName: sourcecode.Name)
    extends AdapterNode(ClockImp)(sourceFn, sinkFn)

case class ClockIdentityNode()(implicit valName: sourcecode.Name) extends IdentityNode(ClockImp)()

case class ClockEphemeralNode()(implicit valName: sourcecode.Name) extends EphemeralNode(ClockImp)()

object ClockNameNode {
  def apply(name: sourcecode.Name): ClockIdentityNode = ClockIdentityNode()(name)
  def apply(name: Option[String]):  ClockIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String):          ClockIdentityNode = apply(Some(name))
}

object ClockTempNode {
  def apply(): ClockEphemeralNode = ClockEphemeralNode()(ValName("temp"))
}

object ClockSinkNode {
  def apply(
    freqMHz:   Double,
    dutyCycle: Double = 50,
    phaseDeg:  Double = 0,
    // Create SDC/TCL constraints that the clock matches these requirements:
    phaseErrorDeg: Double = 5,
    freqErrorPPM:  Double = 10000,
    jitterPS:      Double = 300
  )(
    implicit valName: sourcecode.Name
  ): ClockSinkNode =
    ClockSinkNode(
      Seq(
        ClockSinkParameters(
          phaseDeg = phaseDeg,
          phaseErrorDeg = phaseErrorDeg,
          freqErrorPPM = freqErrorPPM,
          jitterPS = jitterPS,
          take = Some(ClockParameters(freqMHz = freqMHz, dutyCycle = dutyCycle))
        )
      )
    )
}

object ClockSourceNode {
  def apply(
    freqMHz:   Double,
    dutyCycle: Double = 50,
    jitterPS:  Double = 300
  )(
    implicit valName: sourcecode.Name
  ): ClockSourceNode =
    ClockSourceNode(
      Seq(
        ClockSourceParameters(
          jitterPS = Some(jitterPS),
          give = Some(ClockParameters(freqMHz = freqMHz, dutyCycle = dutyCycle))
        )
      )
    )
}

object ClockGroupImp
    extends SimpleNodeImp[
      ClockGroupSourceParameters,
      ClockGroupSinkParameters,
      ClockGroupEdgeParameters,
      ClockGroupBundle
    ] {
  def edge(
    pd:         ClockGroupSourceParameters,
    pu:         ClockGroupSinkParameters,
    sourceInfo: SourceInfo
  ): ClockGroupEdgeParameters =
    ClockGroupEdgeParameters(pd, pu, sourceInfo)
  def bundle(e: ClockGroupEdgeParameters) = new ClockGroupBundle(e.bundle)
  def render(e: ClockGroupEdgeParameters): RenderedEdge = RenderedEdge(colour = "#00cc00" /* green */ )
}

case class ClockGroupSourceNode(params: Seq[ClockGroupSourceParameters])(implicit valName: sourcecode.Name)
    extends SourceNode(ClockGroupImp)(params)
case class ClockGroupSinkNode(params: Seq[ClockGroupSinkParameters])(implicit valName: sourcecode.Name)
    extends SinkNode(ClockGroupImp)(params)

case class ClockGroupAdapterNode(
  sourceFn: ClockGroupSourceParameters => ClockGroupSourceParameters = { m => m },
  sinkFn:   ClockGroupSinkParameters => ClockGroupSinkParameters = { s => s }
)(
  implicit valName: sourcecode.Name)
    extends AdapterNode(ClockGroupImp)(sourceFn, sinkFn)

case class ClockGroupIdentityNode()(implicit valName: sourcecode.Name) extends IdentityNode(ClockGroupImp)()

case class ClockGroupEphemeralNode()(implicit valName: sourcecode.Name) extends EphemeralNode(ClockGroupImp)()
