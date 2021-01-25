package tilelink

case class TLClientPortParameters(
  clients:          Seq[TLClientParameters],
  channelBeatBytes: TLChannelBeatBytes) {
  def endSourceId: Int = clients.map(_.sourceId.end).max

  def maxTransferSizeA: Int = clients.map(_.maxTransferSizeA).max
  def maxTransferSizeB: Int = clients.map(_.maxTransferSizeB).max
  def maxTransferSizeC: Int = clients.map(_.maxTransferSizeC).max
  def maxTransferSizeD: Int = clients.map(_.maxTransferSizeD).max
  def maxTransferSizeE: Int = clients.map(_.maxTransferSizeE).max

  val anySupportAccessAckD:      Boolean = clients.map(!_.supports.accessAckD.none).reduce(_ || _)
  val anySupportAccessAckDataD:  Boolean = clients.map(!_.supports.accessAckDataD.none).reduce(_ || _)
  val anySupportHintAckD:        Boolean = clients.map(!_.supports.hintAckD.none).reduce(_ || _)
  val anySupportProbeBlockB:     Boolean = clients.map(!_.supports.probeBlockB.none).reduce(_ || _)
  val anySupportProbePermB:      Boolean = clients.map(!_.supports.probePermB.none).reduce(_ || _)
  val anySupportGrantD:          Boolean = clients.map(!_.supports.GrantD.none).reduce(_ || _)
  val anySupportGrantDataD:      Boolean = clients.map(!_.supports.GrantDataD.none).reduce(_ || _)
  val anySupportReleaseAckD:     Boolean = clients.map(!_.supports.releaseAckD.none).reduce(_ || _)
  val anySupportGetB:            Boolean = clients.map(!_.supports.getB.none).reduce(_ || _)
  val anySupportPutFullDataB:    Boolean = clients.map(!_.supports.putFullDataB.none).reduce(_ || _)
  val anySupportPutPartialDataB: Boolean = clients.map(!_.supports.putPartialDataB.none).reduce(_ || _)
  val anySupportArithmeticDataB: Boolean = clients.map(!_.supports.arithmeticDataB.none).reduce(_ || _)
  val anySupportLogicalDataB:    Boolean = clients.map(!_.supports.logicalDataB.none).reduce(_ || _)
  val anySupportIntentB:         Boolean = clients.map(!_.supports.intentB.none).reduce(_ || _)
}
