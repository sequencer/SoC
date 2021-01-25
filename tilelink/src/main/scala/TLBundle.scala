package tilelink

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}
import org.chipsalliance.utils.addressing.MaskGen

import scala.collection.immutable.ListMap

case class TLCNotInThisBundle(bundleParameters: TLBundleParameters)
    extends Exception(s"""cannot use TLC with parameter: ${bundleParameters.toString}""")

sealed trait TLChannel extends Bundle {
  val channelParameters: TLChannelParameters
}
sealed trait TLOpcodeChannel { this: TLChannel =>
  val opcode: UInt = UInt(3.W)
  val param:  UInt = UInt(3.W)
}
sealed trait TLDataChannel { this: TLChannel =>
  val data: UInt = UInt(channelParameters.dataWidth.W)
  val size: UInt = UInt(channelParameters.sizeWidth.W)
}
sealed trait TLAddrChannel { this: TLChannel with TLDataChannel =>
  val address: UInt = UInt(channelParameters.addressWidth.W)
}
sealed trait TLSourceChannel { this: TLChannel with TLDataChannel =>
  val source: UInt = UInt(channelParameters.sourceWidth.W)
}
sealed trait TLSinkChannel { this: TLChannel =>
  val sink: UInt = UInt(channelParameters.sinkWidth.W)
}
sealed trait TLMaskChannel { this: TLChannel with TLAddrChannel with TLDataChannel =>
  val mask:    UInt = UInt(channelParameters.maskWidth.W)
  def genMask: UInt = MaskGen(address, size, channelParameters.beatBytes)
}
sealed trait TLCorruptChannel { this: TLChannel =>
  val corrupt: Bool = Bool()
}
sealed trait TLDeniedChannel { this: TLChannel =>
  val denied: Bool = Bool()
}
final class TLChannelA(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    with TLSourceChannel
    with TLAddrChannel
    with TLMaskChannel
    with TLDataChannel
    with TLCorruptChannel
final class TLChannelB(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    with TLSourceChannel
    with TLAddrChannel
    with TLMaskChannel
    with TLDataChannel
    with TLCorruptChannel
final class TLChannelC(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    with TLSourceChannel
    with TLAddrChannel
    with TLDataChannel
    with TLCorruptChannel
final class TLChannelD(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    with TLSourceChannel
    with TLSinkChannel
    with TLDataChannel
    with TLCorruptChannel
    with TLDeniedChannel
final class TLChannelE(val channelParameters: TLChannelParameters) extends TLChannel with TLSinkChannel

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
