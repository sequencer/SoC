package tilelink

case class TLManagerPortParameters(
  managers:         Seq[TLManagerParameters],
  channelBeatBytes: TLChannelBeatBytes,
  endSinkId:        Int) {

  def maxTransferSizeA: Int = managers.map(_.maxTransferSizeA).max
  def maxTransferSizeB: Int = managers.map(_.maxTransferSizeB).max
  def maxTransferSizeC: Int = managers.map(_.maxTransferSizeC).max
  def maxTransferSizeD: Int = managers.map(_.maxTransferSizeD).max

  val anySupportGetA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: GetA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportPutFullDataA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: PutFullDataA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportPutPartialDataA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: PutPartialDataA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportArithmeticDataA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: ArithmeticDataA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportLogicalDataA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: LogicalDataA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportIntentA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: IntentA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportAcquireBlockA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: AcquireBlockA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportAcquirePermA: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: AcquirePermA => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportProbeAckC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: ProbeAckC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportProbeAckDataC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: ProbeAckDataC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportGrantAckE: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: GrantAckE => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportReleaseC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: ReleaseC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportReleaseDataC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: ReleaseDataC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportAccessAckC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: AccessAckC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportAccessAckDataC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: AccessAckDataC => true
      case _ => false
    }
    .reduce(_ || _)
  val anySupportHintAckC: Boolean = managers
    .flatMap(_.supports)
    .map {
      case _: HintAckC => true
      case _ => false
    }
    .reduce(_ || _)

  val anySupportAcquiredB: Boolean = managers
    .flatMap(_.supports)
    .map {
      case a: AcquirePermA  => a.NtoB
      case a: AcquireBlockA => a.NtoB
      case _ => false
    }
    .reduce(_ || _)


  val maxAddress: BigInt = managers.map(_.maxAddress).max
}
