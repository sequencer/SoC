package tilelink

import diplomacy.BaseNode
import org.chipsalliance.utils.addressing.{AddressSet, IdRange}
import org.chipsalliance.utils.dts.Resource

import scala.reflect.{classTag, ClassTag}

case class TLMasterParameters(
  nodePath:    Seq[BaseNode],
  setName:     Option[String],
  sourceId:    IdRange,
  requestFifo: Boolean,
  resources:   Seq[Resource],
  visibility:  Seq[AddressSet],
  supports:    Set[SlaveToMasterMessage],
  emits:       Set[MasterToSlaveMessage]) {

  /** Name of Parameter. if [[setName]] is None, get its name from the [[diplomacy.LazyModule]]. */
  val name: String = setName.orElse(nodePath.lastOption.map((_: BaseNode).lazyModule.name)).getOrElse("disconnected")

  /** get maximum transform size on a specific channel.
    *
    * @tparam Channel can be [[TLChannelA]], [[TLChannelB]], [[TLChannelC]], [[TLChannelD]], [[TLChannelE]]
    */
  def maxTransferSize[Channel <: TLChannel: ClassTag]: Int = {
    val runtimeClass: Class[_] = classTag[Channel].runtimeClass
    if (classOf[TLChannelA].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(emits.filter((_: MasterToSlaveMessage).isInstanceOf[ChannelAMessage]))
    else if (classOf[TLChannelB].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(supports.filter((_: SlaveToMasterMessage).isInstanceOf[ChannelBMessage]))
    else if (classOf[TLChannelC].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(emits.filter((_: MasterToSlaveMessage).isInstanceOf[ChannelCMessage]))
    else if (classOf[TLChannelD].isAssignableFrom(runtimeClass))
      TLMessage.getMaxTransferSizes(supports.filter((_: SlaveToMasterMessage).isInstanceOf[ChannelDMessage]))
    else if (classOf[TLChannelE].isAssignableFrom(runtimeClass))
      0
    else throw new RuntimeException(s"Cannot detect class type for $runtimeClass.")
  }

  /** Get all parameter `T` from [[emits]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[emits]].
    */
  def emits[T <: TLMessage: ClassTag]: Set[T] =
    emits.filter(classTag[T].runtimeClass.isInstance(_: MasterToSlaveMessage)).asInstanceOf[Set[T]]

  /** Get all parameter `T` from [[supports]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[supports]].
    */
  def supports[T <: TLMessage: ClassTag]: Set[T] =
    supports.filter(classTag[T].runtimeClass.isInstance(_: SlaveToMasterMessage)).asInstanceOf[Set[T]]

}
