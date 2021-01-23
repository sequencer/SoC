// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import chisel3._

class ClockBundle(val params: ClockBundleParameters) extends Bundle {
  val clock: Clock = Output(Clock())
  val reset: Reset = Output(Reset())
}

class ClockGroupBundle(val params: ClockGroupBundleParameters) extends Bundle {
  val member: RecordMap[ClockBundle] = RecordMap(params.members.map {
    case (k, v) =>
      k -> new ClockBundle(v)
  })
}
