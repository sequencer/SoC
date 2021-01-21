package org.chipsalliance.utils

import chisel3._
import chisel3.util.{Counter, DecoupledIO, ReadyValidIO, RegEnable}

/**  These wrap behavioral
  *  shift and next registers into specific modules to allow for
  *  backend flows to replace or constrain
  *  them properly when used for CDC synchronization,
  *  rather than buffering.
  *
  *  These are built up of *ResetSynchronizerPrimitiveShiftReg,
  *  intended to be replaced by the integrator's metastable flops chains or replaced
  *  at this level if they have a multi-bit wide synchronizer primitive.
  *  The different types vary in their reset behavior:
  *  NonSyncResetSynchronizerShiftReg    -- Register array which does not have a reset pin
  *  AsyncResetSynchronizerShiftReg      -- Asynchronously reset register array, constructed from W instantiations of D deep
  *                                       1-bit-wide shift registers.
  *  SyncResetSynchronizerShiftReg       -- Synchronously reset register array, constructed similarly to AsyncResetSynchronizerShiftReg
  *
  *  [Inferred]ResetSynchronizerShiftReg -- TBD reset type by chisel3 reset inference.
  *
  *  ClockCrossingReg                    -- Not made up of SynchronizerPrimitiveShiftReg. This is for single-deep flops which cross
  *                                         Clock Domains.
  */
package object crossing {
  def ToAsyncBundle[T <: Data](
    x:      ReadyValidIO[T],
    params: AsyncQueueParams = AsyncQueueParams()
  ): AsyncBundle[T] = {
    val source = Module(new AsyncQueueSource(chiselTypeOf(x.bits), params))
    source.io.enq <> x
    source.io.async
  }

  def FromAsyncBundle[T <: Data](x: AsyncBundle[T]): DecoupledIO[T] =
    FromAsyncBundle(x, x.params.sync)

  def FromAsyncBundle[T <: Data](
    x:    AsyncBundle[T],
    sync: Int
  ): DecoupledIO[T] = {
    val sink = Module(
      new AsyncQueueSink(chiselTypeOf(x.mem(0)), x.params.copy(sync = sync))
    )
    sink.io.async <> x
    sink.io.deq
  }

  def ToRational[T <: Data](
    x:         DecoupledIO[T],
    direction: RationalDirection = Symmetric
  ): RationalIO[T] = {
    val source = Module(
      new RationalCrossingSource(chiselTypeOf(x.bits), direction)
    )
    source.io.enq <> x
    source.io.deq
  }

  def FromRational[T <: Data](
    x:         RationalIO[T],
    direction: RationalDirection = Symmetric
  ): DecoupledIO[T] = {
    val sink = Module(
      new RationalCrossingSink(chiselTypeOf(x.bits0), direction)
    )
    sink.io.enq <> x
    sink.io.deq
  }

  def GrayCounter(
    bits:      Int,
    increment: Bool = true.B,
    clear:     Bool = false.B,
    name:      String = "binary"
  ): UInt = {
    val incremented = Wire(UInt(bits.W))
    val binary = RegNext(next = incremented, init = 0.U).suggestName(name)
    incremented := Mux(clear, 0.U, binary + increment.asUInt())
    incremented ^ (incremented >> 1)
  }

  def ShiftRegInit[T <: Data](
    in:   T,
    n:    Int,
    init: T,
    name: Option[String] = None
  ): T =
    (0 until n).foldRight(in) {
      case (i, next) =>
        val r = RegNext(next, init = init)
        name.foreach { na =>
          r.suggestName(s"${na}_${i}")
        }
        r
    }

  def BlockDuringReset[T <: Data: Blockable](
    data:          T,
    stretchCycles: Int = 0
  ): T = {
    implicitly[Blockable[T]].blockWhile(
      !(stretchCycles match {
        case 0 => RegNext(true.B, false.B)
        case i => RegEnable(true.B, false.B, Counter(true.B, i)._2)
      }),
      data
    )
  }

  object SynchronizerResetType extends Enumeration {
    val NonSync, Inferred, Sync, Async = Value
  }

  val NoCrossing = SynchronousCrossing(BufferParams.none)

  // @todo helpers move to top
  implicit class BooleanToAugmentedBoolean(private val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0
    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }
}
