// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import chisel3.internal.sourceinfo.SourceInfo
import scala.collection.immutable.ListMap

// All Clock parameters specify only the PLL values required at power-on
// Dynamic control of the PLL from software can take the values out-of-range

case class ClockParameters(
  freqMHz:   Double,
  dutyCycle: Double = 50) //in percent
{
  require(freqMHz > 0)
  require(0 < dutyCycle)
  require(dutyCycle < 100)
}

case class ClockSourceParameters(
  jitterPS: Option[Double] = None, // if known at chisel elaboration
  give:     Option[ClockParameters] = None,
  name:     Option[String] = None)

case class ClockSinkParameters(
  phaseDeg: Double = 0,
  // Create SDC/TCL constraints that the clock matches these requirements:
  phaseErrorDeg: Double = 5,
  freqErrorPPM:  Double = 10000,
  jitterPS:      Double = 200,
  take:          Option[ClockParameters] = None,
  name:          Option[String] = None) {
  require(phaseErrorDeg >= 0)
  require(freqErrorPPM >= 0)
}

case class ClockBundleParameters()

case class ClockEdgeParameters(
  source:     ClockSourceParameters,
  sink:       ClockSinkParameters,
  sourceInfo: SourceInfo) {
  // Unify the given+taken ClockParameters
  val clock: Option[ClockParameters] = source.give.orElse(sink.take).map { clock =>
    source.give.foreach { x => require(clock == x) }
    sink.take.foreach { x => require(clock == x) }
    clock
  }

  val bundle: ClockBundleParameters = ClockBundleParameters()
}

// ClockGroups exist as the output of a PLL

case class ClockGroupSourceParameters()
case class ClockGroupSinkParameters(
  name:    String,
  members: Seq[ClockSinkParameters])

case class ClockGroupBundleParameters(
  members: ListMap[String, ClockBundleParameters])

case class ClockGroupEdgeParameters(
  source:     ClockGroupSourceParameters,
  sink:       ClockGroupSinkParameters,
  sourceInfo: SourceInfo) {
  val sourceParameters: ClockSourceParameters = ClockSourceParameters()
  val members: ListMap[String, ClockEdgeParameters] = ListMap(sink.members.zipWithIndex.map {
    case (s, i) =>
      s"${sink.name}_${s.name.getOrElse(i)}" -> ClockEdgeParameters(sourceParameters, s, sourceInfo)
  }: _*)

  val bundle: ClockGroupBundleParameters = ClockGroupBundleParameters(members.map { case (k, v) => k -> v.bundle })
}
