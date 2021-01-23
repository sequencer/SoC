package tilelink
import org.chipsalliance.utils.addressing.TransferSizes

/** Please refer to SiFive TileLink Specification 1.8.1
  *
  * TL-UL
  * @param accessAckB 7.2.4
  * @param accessAckDataB 7.2.5
  * TL-UH
  * @param hintAckD 8.2.4
  * TL-C
  * @param probeBlockB 9.3.3
  * @param probePermB 9.3.4
  * @param GrantD 9.3.7
  * @param GrantDataD 9.3.8
  * @param releaseAckD 9.3.12
  * TL-U in TL-C
  * @param getB 9.5.1
  * @param putFullDataB 9.5.2
  * @param putPartialDataB 9.5.3
  * @param arithmeticDataB 9.5.6
  * @param logicalDataB 9.5.7
  * @param intentB 9.5.8
  */
case class TLManagerToClientSizes(
  accessAckB:      TransferSizes = TransferSizes.none,
  accessAckDataB:  TransferSizes = TransferSizes.none,
  hintAckD:        TransferSizes = TransferSizes.none,
  probeBlockB:     TransferSizes = TransferSizes.none,
  probePermB:      TransferSizes = TransferSizes.none,
  GrantD:          TransferSizes = TransferSizes.none,
  GrantDataD:      TransferSizes = TransferSizes.none,
  releaseAckD:     TransferSizes = TransferSizes.none,
  getB:            TransferSizes = TransferSizes.none,
  putFullDataB:    TransferSizes = TransferSizes.none,
  putPartialDataB: TransferSizes = TransferSizes.none,
  arithmeticDataB: TransferSizes = TransferSizes.none,
  logicalDataB:    TransferSizes = TransferSizes.none,
  intentB:         TransferSizes = TransferSizes.none) {
  def intersect(that: TLManagerToClientSizes): TLManagerToClientSizes =
    TLManagerToClientSizes(
      accessAckB.intersect(that.accessAckB),
      accessAckDataB.intersect(that.accessAckDataB),
      hintAckD.intersect(that.hintAckD),
      probeBlockB.intersect(that.probeBlockB),
      probePermB.intersect(that.probePermB),
      GrantD.intersect(that.GrantD),
      GrantDataD.intersect(that.GrantDataD),
      releaseAckD.intersect(that.releaseAckD),
      getB.intersect(that.getB),
      putFullDataB.intersect(that.putFullDataB),
      putPartialDataB.intersect(that.putPartialDataB),
      arithmeticDataB.intersect(that.arithmeticDataB),
      logicalDataB.intersect(that.logicalDataB),
      intentB.intersect(that.intentB)
    )
  def mincover(that: TLManagerToClientSizes): TLManagerToClientSizes =
    TLManagerToClientSizes(
      accessAckB.mincover(that.accessAckB),
      accessAckDataB.mincover(that.accessAckDataB),
      hintAckD.mincover(that.hintAckD),
      probeBlockB.mincover(that.probeBlockB),
      probePermB.mincover(that.probePermB),
      GrantD.mincover(that.GrantD),
      GrantDataD.mincover(that.GrantDataD),
      releaseAckD.mincover(that.releaseAckD),
      getB.mincover(that.getB),
      putFullDataB.mincover(that.putFullDataB),
      putPartialDataB.mincover(that.putPartialDataB),
      arithmeticDataB.mincover(that.arithmeticDataB),
      logicalDataB.mincover(that.logicalDataB),
      intentB.mincover(that.intentB)
    )

}
