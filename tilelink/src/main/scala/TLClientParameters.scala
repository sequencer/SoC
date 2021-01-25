package tilelink
import diplomacy.BaseNode
import org.chipsalliance.utils.addressing.{AddressSet, IdRange, RegionType}
import org.chipsalliance.utils.dts.Resource

case class TLClientParameters(
  nodePath:          Seq[BaseNode],
  resources:         Seq[Resource],
  setName:           Option[String],
  visibility:        Seq[AddressSet],
  unusedRegionTypes: Set[RegionType.T],
  executesOnly:      Boolean,
  requestFifo:       Boolean,
  supports:          TLManagerToClientSizes,
  emits:             TLClientToManagerSizes,
  neverReleasesData: Boolean,
  sourceId:          IdRange) {

  def maxTransferSizeA: Int = emits.maxTransferSizeA
  def maxTransferSizeB: Int = supports.maxTransferSizeB
  def maxTransferSizeC: Int = emits.maxTransferSizeC
  def maxTransferSizeD: Int = supports.maxTransferSizeD
  def maxTransferSizeE: Int = emits.maxTransferSizeE
}
