package tilelink

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}
import org.chipsalliance.utils.addressing.MaskGen

import scala.collection.immutable.ListMap

case class TLCNotInThisBundle(bundleParameters: TLBundleParameters)
    extends Exception(s"""cannot use TLC with parameter: ${bundleParameters.toString}""")

/** TileLink Spec 1.8.1 Table 14
  *
  * Summary of TileLink routing fields
  *
  * | Channel | Destination | Sequence | Routed By | Provides  | For Use As |
  * | :---:   | :---:       | :---:    | :---:     | :---:     | :---:      |
  * | A       | slave       | request  | a_address | a_source  | d_source   |
  * | B       | master      | request  | b_source  | b_address | c_address  |
  * | C       | slave       | response | c_address | .         |            |
  * | C       | slave       | request  | c_address | c_source  |            |
  * | D       | master      | response | d_source  | .         | d_source   |
  * | D       | master      | request  | d_source  | d_sink    | e_sink     |
  * | E       | slave       | response | e_source  | .         |            |
  */
sealed trait TLChannel extends Bundle {
  val channelParameters: TLChannelParameters
}

/** Apply to ABCD channel. */
sealed trait TLOpcodeChannel { this: TLChannel =>
  val opcode: UInt = UInt(3.W)
  val param:  UInt = UInt(3.W)
}
sealed trait TLDataChannel { this: TLChannel =>
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val corrupt: Bool = Bool()
}
sealed trait TLMaskChannel { this: TLChannel with TLAddressChannel with TLDataChannel =>
  val mask:    UInt = UInt(channelParameters.maskWidth.W)
  def genMask: UInt = MaskGen(address, size, channelParameters.beatBytes)
}

/** only D channel can deny. */
sealed trait TLDeniedChannel { this: TLChannel =>
  val denied: Bool = Bool()
}
sealed trait TLAddressChannel { this: TLChannel with TLDataChannel =>
  val address: UInt = UInt(channelParameters.addressWidth.W)
}
sealed trait TLSourceChannel { this: TLChannel with TLDataChannel =>
  val source: UInt = UInt(channelParameters.sourceWidth.W)
}
sealed trait TLSinkChannel { this: TLChannel =>
  val sink: UInt = UInt(channelParameters.sinkWidth.W)
}
final class TLChannelA(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `a.source` for use as `d.source`
    with TLSourceChannel
    // Request routed by `a.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
final class TLChannelB(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request routed by `b.source`
    with TLSourceChannel
    // Request provides `b.address` for use as `c.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
final class TLChannelC(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `c.source` for use as `d.source`
    with TLSourceChannel
    // Response routed by `c.address` provided by `a.address`
    // Request routed by `c.address`
    with TLAddressChannel
    with TLDataChannel
final class TLChannelD(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Response routed by `d.source` provided by `a.source`
    // Request routed by `d.source`
    with TLSourceChannel
    // Request provides `d.sink` for use as `e.sink`
    with TLSinkChannel
    with TLDataChannel
    with TLDeniedChannel
final class TLChannelE(val channelParameters: TLChannelParameters)
    extends TLChannel
    // Response routed by `e.sink`
    with TLSinkChannel

class TLDecoupledBundle(val bundleParameters: TLBundleParameters) extends Record {
  // TL-UL and TL-UH
  lazy val a: DecoupledIO[TLChannelA] = Decoupled(new TLChannelA(bundleParameters.a))
  lazy val d: DecoupledIO[TLChannelD] = Flipped(Decoupled(new TLChannelD(bundleParameters.d)))
  // TL-C, lazy val will be instantiate at evaluating elements or user call them by mistake.
  lazy val b: DecoupledIO[TLChannelB] =
    if (bundleParameters.isTLC) Flipped(Decoupled(new TLChannelB(bundleParameters.b.get)))
    else throw TLCNotInThisBundle(bundleParameters)
  lazy val c: DecoupledIO[TLChannelC] =
    if (bundleParameters.isTLC) Flipped(Decoupled(new TLChannelC(bundleParameters.c.get)))
    else throw TLCNotInThisBundle(bundleParameters)
  lazy val e: DecoupledIO[TLChannelE] =
    if (bundleParameters.isTLC) Flipped(Decoupled(new TLChannelE(bundleParameters.e.get)))
    else throw TLCNotInThisBundle(bundleParameters)

  override val elements: ListMap[String, DecoupledIO[TLChannel]] =
    if (bundleParameters.isTLC) ListMap("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e)
    else ListMap("a" -> a, "d" -> d)

  override def cloneType: this.type = new TLDecoupledBundle(bundleParameters).asInstanceOf[this.type]
}

object TLBundle {
  def decoupled(bundleParameters: TLBundleParameters) = new TLDecoupledBundle(bundleParameters)
}
