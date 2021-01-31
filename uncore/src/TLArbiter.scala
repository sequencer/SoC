package uncore.tilelink

import chisel3._
import chisel3.util.{Cat, DecoupledIO, Mux1H}
import org.chipsalliance.utils.misc.{EnhancedChisel3Assign, leftOR, rightOR}

/** Arbiter for TileLink. */
object TLArbiter {

  /** @todo doc this. */
  type Policy = (Int, UInt, Bool) => UInt
  val lowestIndexFirst:  Policy = (width, valids, select) => (~(leftOR(valids) << 1)(width - 1, 0)).asUInt()
  val highestIndexFirst: Policy = (width, valids, select) => (~(rightOR(valids) >> 1).pad(width)).asUInt()
  val roundRobin: Policy = (width, valids, select) =>
    if (width == 1) 1.U(1.W)
    else {
      val valid = valids(width - 1, 0)
      assert(valid === valids)
      val mask = RegInit(((BigInt(1) << width) - 1).U(width - 1, 0))
      val filter = (valid & (~mask).asUInt()) ## valid
      val unready = (rightOR(filter, width * 2, width) >> 1).asUInt() | (mask << width).asUInt()
      val readys = (~((unready >> width).asUInt() & unready(width - 1, 0))).asUInt()
      when(select && valid.orR) {
        mask := leftOR(readys & valid, width)
      }
      readys(width - 1, 0)
    }

  def apply[T <: Data](policy: Policy)(sink: DecoupledIO[T], sources: (UInt, DecoupledIO[T])*) {
    if (sources.isEmpty) {
      sink.valid := false.B
    } else if (sources.size == 1) {
      sink :<> sources.head._2
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(0.U)
      val idle = beatsLeft === 0.U
      val latch = idle && sink.ready // winner (if any) claims sink

      // Who wants access to the sink?
      val valids = sourcesIn.map(_.valid)
      // Arbitrate amongst the requests
      val readys = VecInit(policy(valids.size, Cat(valids.reverse), latch).asBools)
      // Which request wins arbitration?
      val winner = VecInit(readys.zip(valids).map { case (r, v) => r && v })

      // Confirm the policy works properly
      require(readys.size == valids.size)
      // Never two winners
      val prefixOR = winner.scanLeft(false.B)(_ || _).init
      assert(prefixOR.zip(winner).map { case (p, w) => !p || !w }.reduce { _ && _ })
      // If there was any request, there is a winner
      assert(!valids.reduce(_ || _) || winner.reduce(_ || _))

      // Track remaining beats
      val maskedBeats = winner.zip(beatsIn).map { case (w, b) => Mux(w, b, 0.U) }
      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire())

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      sourcesIn.zip(allowed).foreach {
        case (s, r) =>
          s.ready := sink.ready && r
      }
      sink.valid := Mux(idle, valids.reduce(_ || _), Mux1H(state, valids))
      sink.bits :<= Mux1H(muxState, sourcesIn.map(_.bits))
    }
  }

}
