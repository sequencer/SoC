package tilelink

import chisel3._
import chisel3.util.{log2Ceil, log2Up, Decoupled, DecoupledIO}

import scala.collection.immutable.ListMap
import scala.math.max

case class TLBundleParameters(
  a: TLChannelParameters = TLChannelParameters(),
  b: Option[TLChannelParameters] = None,
  c: Option[TLChannelParameters] = None,
  d: TLChannelParameters = TLChannelParameters(),
  e: Option[TLChannelParameters] = None) {
  require(
    (b.isDefined && c.isDefined && e.isDefined) || (b.isEmpty && c.isEmpty && e.isEmpty),
    "only AD/ABCDE channels is allowed."
  )

  val isTLC: Boolean = b.isDefined && c.isDefined && e.isDefined

  def union(that: TLBundleParameters): TLBundleParameters = {
    TLBundleParameters(
      TLChannelParameters.union(a, that.a),
      TLChannelParameters.union(b, that.b),
      TLChannelParameters.union(c, that.c),
      TLChannelParameters.union(d, that.d),
      TLChannelParameters.union(e, that.e)
    )
  }

}

object TLBundleParameters {
  def apply(client: TLMasterPortParameters, manager: TLSlavePortParameters): TLBundleParameters =
    if (
      (client.supports[ProbeBlockB].nonEmpty || client.supports[ProbePermB].nonEmpty) &&
      ((manager.supports[AcquireBlockA].nonEmpty || manager.supports[AcquirePermA].nonEmpty) ||
      (manager.supports[ReleaseC].nonEmpty || manager.supports[ReleaseDataC].nonEmpty))
    )
      TLBundleParameters(
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.a * 8,
          addressWidth = log2Up(manager.maxAddress + 1),
          sourceWidth = log2Up(client.endSourceId),
          sizeWidth = log2Up(log2Ceil(max(client.maxTransferSize[TLChannelA], manager.maxTransferSize[TLChannelA])) + 1)
        ),
        Some(
          TLChannelParameters(
            dataWidth = manager.channelBeatBytes.b * 8,
            addressWidth = log2Up(manager.maxAddress + 1),
            sourceWidth = log2Up(client.endSourceId),
            sizeWidth =
              log2Up(log2Ceil(max(client.maxTransferSize[TLChannelB], manager.maxTransferSize[TLChannelB])) + 1)
          )
        ),
        Some(
          TLChannelParameters(
            dataWidth = manager.channelBeatBytes.c * 8,
            sizeWidth =
              log2Up(log2Ceil(max(client.maxTransferSize[TLChannelC], manager.maxTransferSize[TLChannelC])) + 1),
            addressWidth = log2Up(manager.maxAddress + 1),
            sourceWidth = log2Up(client.endSourceId),
            sinkWidth = log2Up(manager.endSinkId)
          )
        ),
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.d * 8,
          sizeWidth =
            log2Up(log2Ceil(max(client.maxTransferSize[TLChannelD], manager.maxTransferSize[TLChannelD])) + 1),
          sourceWidth = log2Up(client.endSourceId),
          sinkWidth = log2Up(manager.endSinkId)
        ),
        Some(
          TLChannelParameters(
            sinkWidth = log2Up(manager.endSinkId)
          )
        )
      )
    else
      TLBundleParameters(
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.a * 8,
          sizeWidth =
            log2Up(log2Ceil(max(client.maxTransferSize[TLChannelA], manager.maxTransferSize[TLChannelA])) + 1),
          addressWidth = log2Up(manager.maxAddress + 1),
          sourceWidth = log2Up(client.endSourceId)
        ),
        None,
        None,
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.d * 8,
          sizeWidth =
            log2Up(log2Ceil(max(client.maxTransferSize[TLChannelD], manager.maxTransferSize[TLChannelD])) + 1),
          sourceWidth = log2Up(client.endSourceId),
          sinkWidth = log2Up(manager.endSinkId)
        ),
        None
      )

  def union(x: Seq[TLBundleParameters]): TLBundleParameters =
    x.reduce((x: TLBundleParameters, y: TLBundleParameters) => x.union(y))
}

case class TLCNotInThisBundle(bundleParameters: TLBundleParameters)
    extends Exception(s"""cannot use TLC with parameter: ${bundleParameters.toString}""")

class TLDecoupledBundle(val bundleParameters: TLBundleParameters) extends Record {
  // TL-UL and TL-UH
  lazy val a: DecoupledIO[TLChannelA] = Decoupled(new TLChannelA(bundleParameters.a))
  lazy val d: DecoupledIO[TLChannelD] = Flipped(Decoupled(new TLChannelD(bundleParameters.d)))

  // TL-C, lazy val will be instantiate at evaluating [[elements]] or user call them by mistake.
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
