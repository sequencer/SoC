package org.chipsalliance.utils.crossing

import chisel3._

/** Divide the clock by power of 2 times.
  *
  * @param pow2 divides the clock 2 ^ pow2 times
  */
class Pow2ClockDivider(pow2: Int) extends Module {
  class Pow2ClockDividerBundle extends Bundle {
    val clock_out = Output(Clock())
  }
  val io = IO(new Pow2ClockDividerBundle)

  if (pow2 == 0) {
    io.clock_out := clock
  } else {
    val dividers = Seq.fill(pow2) {
      Module(new ClockDivider2)
    }

    dividers.init.zip(dividers.tail).map {
      case (last, next) =>
        next.io.clk_in := last.io.clk_out
    }

    dividers.head.io.clk_in := clock
    io.clock_out := dividers.last.io.clk_out
  }
}

object Pow2ClockDivider {
  def apply(pow2: Int): Clock = Module(new Pow2ClockDivider(pow2)).io.clock_out

  def apply(clock_in: Clock, pow2: Int): Clock = withClock(clock_in) {
    apply(pow2)
  }
}
