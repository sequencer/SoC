package tilelink

import scala.reflect.{classTag, ClassTag}

/**
  * @note [[TLChannelBeatBytes]] is provided by [[TLSlavePortParameters]]
  */
case class TLMasterPortParameters(
  masters: Vector[TLMasterParameters]) {

  /** all possible transaction support by this port. */
  val supports: Set[SlaveToMasterMessage] = masters
    .flatMap((_: TLMasterParameters).supports)
    .groupBy((_: SlaveToMasterMessage).getClass())
    .map {
      case (_, v: Vector[SlaveToMasterMessage]) =>
        v.reduce((_: SlaveToMasterMessage).union(_: SlaveToMasterMessage))
    }
    .toSet

  /** all possible transaction emit by this port. */
  val emits: Set[MasterToSlaveMessage] = masters
    .flatMap((_: TLMasterParameters).emits)
    .groupBy((_: MasterToSlaveMessage).getClass())
    .map {
      case (_, v: Vector[MasterToSlaveMessage]) =>
        v.reduce((_: MasterToSlaveMessage).union(_: MasterToSlaveMessage))
    }
    .toSet

  /** the maximum source id on this port. */
  val endSourceId: Int = masters.map((_: TLMasterParameters).sourceId.end).max

  /** get maximum transform size on a specific port.
    *
    * @tparam Channel can be [[TLChannelA]], [[TLChannelB]], [[TLChannelC]], [[TLChannelD]], [[TLChannelE]]
    */
  def maxTransferSize[Channel <: TLChannel: ClassTag]: Int =
    masters.map((_: TLMasterParameters).maxTransferSize[Channel]).max

  /** Get all parameter `T` from [[emits]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[emits]].
    */
  def supports[T <: TLMessage: ClassTag]: Set[T] =
    supports.filter(classTag[T].runtimeClass.isInstance(_: SlaveToMasterMessage)).asInstanceOf[Set[T]]

  /** Get all parameter `T` from [[supports]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[supports]].
    */
  def emits[T <: TLMessage: ClassTag]: Set[T] =
    emits.filter(classTag[T].runtimeClass.isInstance(_: MasterToSlaveMessage)).asInstanceOf[Set[T]]
}
