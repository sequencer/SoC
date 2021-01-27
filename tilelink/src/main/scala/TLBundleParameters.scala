package tilelink

import chisel3.util.{log2Ceil, log2Up}

import scala.math.max

case class TLBundleParameters(
  a: TLChannelParameters,
  b: Option[TLChannelParameters],
  c: Option[TLChannelParameters],
  d: TLChannelParameters,
  e: Option[TLChannelParameters]) {
  require(
    (b.isDefined && c.isDefined && e.isDefined) || (b.isEmpty && c.isEmpty && e.isEmpty),
    "only AD/ABCDE channels is allowed."
  )
  def isTLC: Boolean = b.isDefined && c.isDefined && e.isDefined
}

object TLBundleParameters {
  def apply(client: TLClientPortParameters, manager: TLManagerPortParameters): TLBundleParameters =
    if (
      (client.anySupportProbeBlockB || client.anySupportProbePermB) &&
      ((manager.anySupportAcquireBlockA || manager.anySupportAcquirePermA) ||
      (manager.anySupportReleaseC || manager.anySupportReleaseDataC))
    )
      TLBundleParameters(
        TLChannelParameters(
          addressWidth = log2Up(manager.maxAddress + 1),
          dataWidth = manager.channelBeatBytes.a * 8,
          sourceWidth = log2Up(client.endSourceId),
          sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeA, manager.maxTransferSizeA)) + 1)
        ),
        None,
        None,
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.d * 8,
          sourceWidth = log2Up(client.endSourceId),
          sinkWidth = log2Up(manager.endSinkId),
          sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeC, manager.maxTransferSizeC)) + 1)
        ),
        None
      )
    else
      TLBundleParameters(
        TLChannelParameters(
          addressWidth = log2Up(manager.maxAddress + 1),
          dataWidth = manager.channelBeatBytes.a * 8,
          sourceWidth = log2Up(client.endSourceId),
          sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeA, manager.maxTransferSizeA)) + 1)
        ),
        Some(
          TLChannelParameters(
            addressWidth = log2Up(manager.maxAddress + 1),
            dataWidth = manager.channelBeatBytes.b * 8,
            sourceWidth = log2Up(client.endSourceId),
            sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeB, manager.maxTransferSizeB)) + 1)
          )
        ),
        Some(
          TLChannelParameters(
            addressWidth = log2Up(manager.maxAddress + 1),
            dataWidth = manager.channelBeatBytes.c * 8,
            sourceWidth = log2Up(client.endSourceId),
            sinkWidth = log2Up(manager.endSinkId),
            sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeC, manager.maxTransferSizeC)) + 1)
          )
        ),
        TLChannelParameters(
          dataWidth = manager.channelBeatBytes.d * 8,
          sourceWidth = log2Up(client.endSourceId),
          sinkWidth = log2Up(manager.endSinkId),
          sizeWidth = log2Up(log2Ceil(max(client.maxTransferSizeD, manager.maxTransferSizeD)) + 1)
        ),
        Some(
          TLChannelParameters(
            sinkWidth = log2Up(manager.endSinkId)
          )
        )
      )
}
