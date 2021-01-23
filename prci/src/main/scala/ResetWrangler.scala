// See LICENSE for license details.
package org.chipsalliance.utils.prci

import chisel3._
import chisel3.util._
import diplomacy.config._
import diplomacy._
import org.chipsalliance.utils.crossing.{AsyncResetReg, ResetCatchAndSync}

class ResetWrangler(debounceNs: Double = 100000)(implicit p: Parameters) extends LazyModule {
  val node: ClockAdapterNode = ClockAdapterNode()

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    val (in, _) = node.in.unzip
    val (out, _) = node.out.unzip

    val status: UInt = IO(Output(UInt(in.size.W)))
    status := Cat(in.map(_.reset.asBool).reverse)
    val causes: Bool = in.map(_.reset).foldLeft(false.B)(_.asBool || _.asBool)

    require(node.in.forall(_._2.clock.isDefined), "Cannot wrangle reset for an unspecified clock frequency")
    val (slowIn, slowEdge) = node.in.minBy(_._2.clock.get.freqMHz)
    val slowPeriodNs: Double = 1000 / slowEdge.clock.get.freqMHz
    val slowTicks:    Int = math.ceil(debounceNs / slowPeriodNs).toInt.max(7)
    val slowBits:     Int = log2Ceil(slowTicks + 1)

    // debounce
    val increment:   Bool = Wire(Bool())
    val incremented: UInt = Wire(UInt(slowBits.W))
    val debounced: UInt = withClockAndReset(slowIn.clock, causes) {
      AsyncResetReg(incremented, 0, increment, Some("debounce"))
    }
    increment := debounced =/= slowTicks.U
    incremented := debounced + 1.U
    val deglitched: Bool = AsyncResetReg(increment, slowIn.clock, causes, init = true, Some("deglitch"))

    // catch and sync increment to each domain
    in.zip(out).foreach {
      case (i, o) =>
        o.clock := i.clock
        o.reset := ResetCatchAndSync(o.clock, deglitched)
    }
  }
}
