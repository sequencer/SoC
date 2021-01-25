package tilelink

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

import scala.collection.immutable.ListMap

case class TLCNotInThisBundle(bundleParameters: TLBundleParameters)
    extends Exception(s"""cannot use TLC with parameter: ${bundleParameters.toString}""")

sealed trait TLChannel extends Bundle {
  val channelName: String
}
sealed trait TLDataChannel extends TLChannel
sealed trait TLAddrChannel extends TLChannel

final class TLChannelA(channelParameters: TLChannelParameters) extends TLChannel with TLAddrChannel with TLDataChannel {
  val channelName = "'A' channel"
  val opcode:  UInt = UInt(3.W)
  val param:   UInt = UInt(3.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val source:  UInt = UInt(channelParameters.sourceWidth.W)
  val address: UInt = UInt(channelParameters.addressWidth.W)
  val mask:    UInt = UInt(channelParameters.maskWidth.W)
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val corrupt: Bool = Bool()
}
final class TLChannelB(channelParameters: TLChannelParameters) extends TLChannel with TLAddrChannel with TLDataChannel {
  val channelName = "'B' channel"
  val opcode:  UInt = UInt(3.W)
  val param:   UInt = UInt(3.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val source:  UInt = UInt(channelParameters.sourceWidth.W)
  val address: UInt = UInt(channelParameters.addressWidth.W)
  val mask:    UInt = UInt(channelParameters.maskWidth.W)
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val corrupt: Bool = Bool()
}
final class TLChannelC(channelParameters: TLChannelParameters) extends TLChannel with TLAddrChannel with TLDataChannel {
  val channelName = "'C' channel"
  val opcode:  UInt = UInt(3.W)
  val param:   UInt = UInt(3.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val source:  UInt = UInt(channelParameters.sourceWidth.W)
  val address: UInt = UInt(channelParameters.addressWidth.W)
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val corrupt: Bool = Bool()
}
final class TLChannelD(channelParameters: TLChannelParameters) extends TLChannel with TLDataChannel {
  val channelName = "'D' channel"
  val opcode:  UInt = UInt(3.W)
  val param:   UInt = UInt(3.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val source:  UInt = UInt(channelParameters.sourceWidth.W)
  val sink:    UInt = UInt(channelParameters.sinkWidth.W)
  val denied:  Bool = Bool()
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val corrupt: Bool = Bool()

}
final class TLChannelE(channelParameters: TLChannelParameters) extends TLChannel {
  val channelName = "'E' channel"
  val sink: UInt = UInt(channelParameters.sinkWidth.W)
}

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
