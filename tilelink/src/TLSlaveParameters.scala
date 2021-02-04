package tilelink

import diplomacy.BaseNode
import org.chipsalliance.utils.addressing.{AddressSet, RegionType}
import org.chipsalliance.utils.dts.{Resource, ResourceAddress, ResourcePermissions}

import scala.reflect.{classTag, ClassTag}

case class TLSlaveParameters(
  nodePath:   Seq[BaseNode],
  resources:  Seq[Resource],
  setName:    Option[String],
  address:    Seq[AddressSet],
  regionType: RegionType.T,
  executable: Boolean,
  fifoDomain: Option[Int],
  supports:   Set[MasterToSlaveMessage],
  emits:      Set[SlaveToMasterMessage]) {

  /** Name of Parameter. if [[setName]] is None, get its name from the [[diplomacy.LazyModule]]. */
  val name: String = setName.orElse(nodePath.lastOption.map((_: BaseNode).lazyModule.name)).getOrElse("disconnected")

  /** maximum address */
  val maxAddress: BigInt = address.map((_: AddressSet).max).max

  /** minimal address alignment. */
  val minAlignment: BigInt = address.map((_: AddressSet).alignment).min

  /** get maximum transform size on a specific channel.
    *
    * @tparam Channel can be [[TLChannelA]], [[TLChannelB]], [[TLChannelC]], [[TLChannelD]], [[TLChannelE]]
    */
  def maxTransferSize[Channel <: TLChannel: ClassTag]: Int = {
    val runtimeClass: Class[_] = classTag[Channel].runtimeClass
    if (classOf[TLChannelA].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(supports.filter((_: MasterToSlaveMessage).isInstanceOf[ChannelAMessage]))
    else if (classOf[TLChannelB].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(emits.filter((_: SlaveToMasterMessage).isInstanceOf[ChannelBMessage]))
    else if (classOf[TLChannelC].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(supports.filter((_: MasterToSlaveMessage).isInstanceOf[ChannelCMessage]))
    else if (classOf[TLChannelD].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(emits.filter((_: SlaveToMasterMessage).isInstanceOf[ChannelDMessage]))
    else if (classOf[TLChannelE].isAssignableFrom(runtimeClass))
      0
    else throw new RuntimeException(s"Cannot detect class type for $runtimeClass.")
  }

  /** DTS resource
    *
    * @note
    * API here has some thing to concern: all `supports` and `emits` are possible to support or possible to emit.
    * But not the final negotiation result.
    * Which will result a special case:
    * After diplomacy negotiations, the resource is not writeable since no master will write.
    * So there still needs a phase of lazy evaluation to get the intersect between this.support and that.emit.
    */
  def toResource: ResourceAddress = {
    ResourceAddress(
      address,
      ResourcePermissions(
        readable =
          supports[GetA].nonEmpty ||
            // AcquireBlockA | AcquirePermA
            supports[HasGrow].nonEmpty,
        writeable =
          // AcquireBlockA | AcquirePermA can grow to Tip.
          supports[PutFullDataA].nonEmpty ||
            supports[PutPartialDataA].nonEmpty ||
            supports[HasGrow].exists((tlMessage: HasGrow) => tlMessage.NtoT || tlMessage.BtoT),
        executable = executable,
        cacheable =
          // AcquireBlockA | AcquirePermA
          supports[HasGrow].nonEmpty,
        atomic = supports[ArithmeticDataA].nonEmpty && supports[LogicalDataA].nonEmpty
      )
    )
  }

  /** Get all parameter `T` from [[supports]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[emits]].
    */
  def emits[T <: TLMessage: ClassTag]: Set[T] =
    emits.filter(classTag[T].runtimeClass.isInstance(_: SlaveToMasterMessage)).asInstanceOf[Set[T]]

  /** Get all parameter `T` from [[emits]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[supports]].
    */
  def supports[T <: TLMessage: ClassTag]: Set[T] =
    supports.filter(classTag[T].runtimeClass.isInstance(_: MasterToSlaveMessage)).asInstanceOf[Set[T]]

  /** address related sanity check. */
  private def checkAddress(): Unit = {
    require(address.nonEmpty, s"Address of $this cannot be empty")
    address.foreach { a: AddressSet => require(a.finite, s"Address $this must be finite") }
    address.combinations(2).foreach {
      case Seq(x, y) => require(!x.overlaps(y), s"Address in $this: $x and $y overlap.")
    }
  }

  checkAddress()
}
