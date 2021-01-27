package tilelink

/**
  * @note [[TLChannelBeatBytes]] is provided by [[TLManagerPortParameters]]
  */
case class TLClientPortParameters(
  clients: Seq[TLClientParameters]) {
  def endSourceId: Int = clients.map(_.sourceId.end).max

  def maxTransferSizeA: Int = clients.map(_.maxTransferSizeA).max
  def maxTransferSizeB: Int = clients.map(_.maxTransferSizeB).max
  def maxTransferSizeC: Int = clients.map(_.maxTransferSizeC).max
  def maxTransferSizeD: Int = clients.map(_.maxTransferSizeD).max

  val anySupportAccessAckD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: AccessAckD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportAccessAckDataD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: AccessAckDataD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportHintAckD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: HintAckD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportProbeBlockB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: ProbeBlockB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportProbePermB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: ProbePermB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportGrantD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: GrantD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportGrantDataD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: GrantDataD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportReleaseAckD: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: ReleaseAckD => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportGetB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: GetB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportPutFullDataB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: PutFullDataB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportPutPartialDataB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: PutPartialDataB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportArithmeticDataB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: ArithmeticDataB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportLogicalDataB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: LogicalDataB => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportIntentB: Boolean = clients
    .flatMap(_.supports)
    .map {
      case _: IntentB => true
      case _ => false
    }
    .reduce(_ || _)

  val anySupportProbe: Boolean = anySupportProbeBlockB || anySupportProbePermB
}
