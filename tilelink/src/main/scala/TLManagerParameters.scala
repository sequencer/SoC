package tilelink
import diplomacy.BaseNode
import org.chipsalliance.utils.addressing.{AddressSet, RegionType}
import org.chipsalliance.utils.dts.{Resource, ResourceAddress, ResourcePermissions}

case class TLManagerParameters(
  nodePath:        Seq[BaseNode],
  resources:       Seq[Resource],
  setName:         Option[String],
  address:         Seq[AddressSet],
  regionType:      RegionType.T,
  supports:        TLClientToManagerSizes,
  emits:           TLManagerToClientSizes,
  executable:      Boolean,
  fifoDomain:      Option[Int],
  supportAcquireT: Boolean,
  supportAcquireB: Boolean,
  alwaysGrantsT:   Boolean,
  mayDenyGet:      Boolean,
  mayDenyPut:      Boolean) {
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
        readable = supportAcquireT || supports.getA,
        writeable = supportAcquireT || supports.putFullDataA || supports.putPartialDataA,
        executable = executable,
        cacheable = supportAcquireB,
        atomic = supports.arithmeticDataA && supports.logicDataA
      )
    )
  }

  checkAddress()
}
