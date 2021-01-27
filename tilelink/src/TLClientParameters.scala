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
  supports:          Set[ManagerToClientTransaction],
  emits:             Set[ClientToManagerTransaction],
  neverReleasesData: Boolean,
  sourceId:          IdRange) {
  val name: String = setName.orElse(nodePath.lastOption.map(_.lazyModule.name)).getOrElse("disconnected")

  def maxTransferSizeA: Int = emits.collect {
    case a: ChannelATransaction => a
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeB: Int = supports.collect {
    case c: ChannelBTransaction => c
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeC: Int = emits.collect {
    case c: ChannelCTransaction => c
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeD: Int = supports.collect {
    case d: ChannelDTransaction => d
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
}
