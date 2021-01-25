package tilelink

case class TLManagerPortParameters(
  managers:         Seq[TLManagerParameters],
  channelBeatBytes: TLChannelBeatBytes,
  endSinkId:        BigInt) {

  def maxTransferSizeA: Int = managers.map(_.maxTransferSizeA).max
  def maxTransferSizeB: Int = managers.map(_.maxTransferSizeB).max
  def maxTransferSizeC: Int = managers.map(_.maxTransferSizeC).max
  def maxTransferSizeD: Int = managers.map(_.maxTransferSizeD).max
  def maxTransferSizeE: Int = managers.map(_.maxTransferSizeE).max

  val anySupportClaims: TLClientToManagerSizes = managers.map(_.supports).reduce(_ mincover _)

  val anySupportGetA:            Boolean = !anySupportClaims.getA.none
  val anySupportPutFullDataA:    Boolean = !anySupportClaims.putFullDataA.none
  val anySupportPutPartialDataA: Boolean = !anySupportClaims.putPartialDataA.none
  val anySupportArithmeticDataA: Boolean = !anySupportClaims.arithmeticDataA.none
  val anySupportLogicDataA:      Boolean = !anySupportClaims.logicDataA.none
  val anySupportIntentA:         Boolean = !anySupportClaims.intentA.none
  val anySupportAcquireBlockA:   Boolean = !anySupportClaims.acquireBlockA.none
  val anySupportAcquirePermA:    Boolean = !anySupportClaims.acquirePermA.none
  val anySupportProbeAckC:       Boolean = !anySupportClaims.probeAckC.none
  val anySupportProbeAckDataC:   Boolean = !anySupportClaims.probeAckDataC.none
  val anySupportGrantAckE:       Boolean = !anySupportClaims.grantAckE.none
  val anySupportReleaseC:        Boolean = !anySupportClaims.releaseC.none
  val anySupportReleaseDataC:    Boolean = !anySupportClaims.releaseDataC.none
  val anySupportAccessAckC:      Boolean = !anySupportClaims.accessAckC.none
  val anySupportAccessAckDataC:  Boolean = !anySupportClaims.accessAckDataC.none
  val anySupportHintAckC:        Boolean = !anySupportClaims.hintAckC.none

  val maxAddress: BigInt = managers.map(_.maxAddress).max
}
