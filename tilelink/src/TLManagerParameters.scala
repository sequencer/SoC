package tilelink
import diplomacy.BaseNode
import org.chipsalliance.utils.addressing.{AddressSet, RegionType}
import org.chipsalliance.utils.dts.{Resource, ResourceAddress, ResourcePermissions}

case class TLManagerParameters(
  nodePath:   Seq[BaseNode],
  resources:  Seq[Resource],
  setName:    Option[String],
  address:    Seq[AddressSet],
  regionType: RegionType.T,
  executable: Boolean,
  fifoDomain: Option[Int],
  supports:   Set[ClientToManagerTransaction],
  emits:      Set[ManagerToClientTransaction]) {

  val supportGetA: Boolean = supports.map {
    case _: GetA => true
    case _ => false
  }.reduce(_ || _)
  val supportArithmeticDataA: Boolean = supports.map {
    case _: ArithmeticDataA => true
    case _ => false
  }.reduce(_ || _)
  val supportLogicalDataA: Boolean = supports.map {
    case _: LogicalDataA => true
    case _ => false
  }.reduce(_ || _)
  val supportPutFullDataA: Boolean = supports.map {
    case _: PutFullDataA => true
    case _ => false
  }.reduce(_ || _)
  val supportPutPartialDataA: Boolean = supports.map {
    case _: PutPartialDataA => true
    case _ => false
  }.reduce(_ || _)
  val supportAcquireT: Boolean = supports.map {
    case a: AcquireBlockA => a.BtoT | a.NtoT
    case a: AcquirePermA  => a.BtoT | a.NtoT
    case _ => false
  }.reduce(_ || _)
  val supportAcquireB: Boolean = supports.map {
    case a: AcquireBlockA => a.NtoB
    case a: AcquirePermA  => a.NtoB
    case _ => false
  }.reduce(_ || _)
  val alwaysGrantsT: Boolean = emits.map {
    case a: GrantD     => !a.toB && !a.toN && a.toT
    case a: GrantDataD => !a.toB && !a.toN && a.toT
    case _ => false
  }.reduce(_ && _)
  val mayDeny: Set[ManagerToClientTransaction with HasDeny] = emits.collect {
    case a: HasDeny => a
  }

  def maxTransferSizeA: Int = supports.collect {
    case a: ChannelATransaction => a
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeB: Int = emits.collect {
    case c: ChannelBTransaction => c
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeC: Int = supports.collect {
    case c: ChannelCTransaction => c
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max
  def maxTransferSizeD: Int = emits.collect {
    case d: ChannelDTransaction => d
  }.collect {
    case t: HasTransferSizes => t.transferSizes.max
  }.max

  private def checkAddress(): Unit = {
    require(address.nonEmpty, s"Address of $this cannot be empty")
    address.foreach { a => require(a.finite, s"Address $this must be finite") }
    address.combinations(2).foreach {
      case Seq(x, y) => require(!x.overlaps(y), s"Address in $this: $x and $y overlap.")
    }
  }

  val name:         String = setName.orElse(nodePath.lastOption.map(_.lazyModule.name)).getOrElse("disconnected")
  val maxAddress:   BigInt = address.map(_.max).max
  val minAlignment: BigInt = address.map(_.alignment).min

  def toResource: ResourceAddress = {
    ResourceAddress(
      address,
      ResourcePermissions(
        readable = supportAcquireT || supportGetA,
        writeable = supportAcquireT || supportPutFullDataA || supportPutPartialDataA,
        executable = executable,
        cacheable = supportAcquireB,
        atomic = supportArithmeticDataA && supportLogicalDataA
      )
    )
  }

  checkAddress()
}
