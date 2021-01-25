package tilelink
import org.chipsalliance.utils.addressing.TransferSizes

/** Please refer to SiFive TileLink Specification 1.8.1
  *
  * TL-UL
  * @param getA 7.2.1
  * @param putFullDataA 7.2.2
  * @param putPartialDataA 7.2.3
  * TL-UH
  * @param arithmeticDataA 8.2.1
  * @param logicDataA 8.2.2
  * @param intentA 8.2.3
  * TL-C
  * @param acquireBlockA 9.3.1
  * @param acquirePermA 9.3.2
  * @param probeAckC 9.3.5
  * @param probeAckDataC 9.3.6
  * @param grantAckE 9.3.9
  * @param releaseC 9.3.10
  * @param releaseDataC 9.3.11
  * TL-U in TL-C
  * @param accessAckC 9.5.4
  * @param accessAckDataC 9.5.5
  * @param hintAckC 9.5.9
  */
case class TLClientToManagerSizes(
  getA:            TransferSizes = TransferSizes.none,
  putFullDataA:    TransferSizes = TransferSizes.none,
  putPartialDataA: TransferSizes = TransferSizes.none,
  arithmeticDataA: TransferSizes = TransferSizes.none,
  logicDataA:      TransferSizes = TransferSizes.none,
  intentA:         TransferSizes = TransferSizes.none,
  acquireBlockA:   TransferSizes = TransferSizes.none,
  acquirePermA:    TransferSizes = TransferSizes.none,
  probeAckC:       TransferSizes = TransferSizes.none,
  probeAckDataC:   TransferSizes = TransferSizes.none,
  grantAckE:       TransferSizes = TransferSizes.none,
  releaseC:        TransferSizes = TransferSizes.none,
  releaseDataC:    TransferSizes = TransferSizes.none,
  accessAckC:      TransferSizes = TransferSizes.none,
  accessAckDataC:  TransferSizes = TransferSizes.none,
  hintAckC:        TransferSizes = TransferSizes.none) {
  def maxTransferSizeA = List(
    getA.max,
    putFullDataA.max,
    putPartialDataA.max,
    arithmeticDataA.max,
    logicDataA.max,
    intentA.max,
    acquireBlockA.max,
    acquirePermA.max
  ).max

  def maxTransferSizeC = List(
    probeAckC.max,
    probeAckDataC.max,
    releaseC.max,
    releaseDataC.max,
    accessAckC.max,
    accessAckDataC.max,
    hintAckC.max
  ).max

  def maxTransferSizeE = List(
    grantAckE.max
  ).max

  def intersect(that: TLClientToManagerSizes): TLClientToManagerSizes =
    TLClientToManagerSizes(
      getA.intersect(that.getA),
      putFullDataA.intersect(that.putFullDataA),
      putPartialDataA.intersect(that.putPartialDataA),
      arithmeticDataA.intersect(that.arithmeticDataA),
      logicDataA.intersect(that.logicDataA),
      intentA.intersect(that.intentA),
      acquireBlockA.intersect(that.acquireBlockA),
      acquirePermA.intersect(that.acquirePermA),
      probeAckC.intersect(that.probeAckC),
      probeAckDataC.intersect(that.probeAckDataC),
      grantAckE.intersect(that.grantAckE),
      releaseC.intersect(that.releaseC),
      releaseDataC.intersect(that.releaseDataC),
      accessAckC.intersect(that.accessAckC),
      accessAckDataC.intersect(that.accessAckDataC),
      hintAckC.intersect(that.hintAckC)
    )
  def mincover(that: TLClientToManagerSizes): TLClientToManagerSizes =
    TLClientToManagerSizes(
      getA.mincover(that.getA),
      putFullDataA.mincover(that.putFullDataA),
      putPartialDataA.mincover(that.putPartialDataA),
      arithmeticDataA.mincover(that.arithmeticDataA),
      logicDataA.mincover(that.logicDataA),
      intentA.mincover(that.intentA),
      acquireBlockA.mincover(that.acquireBlockA),
      acquirePermA.mincover(that.acquirePermA),
      probeAckC.mincover(that.probeAckC),
      probeAckDataC.mincover(that.probeAckDataC),
      grantAckE.mincover(that.grantAckE),
      releaseC.mincover(that.releaseC),
      releaseDataC.mincover(that.releaseDataC),
      accessAckC.mincover(that.accessAckC),
      accessAckDataC.mincover(that.accessAckDataC),
      hintAckC.mincover(that.hintAckC)
    )
}
