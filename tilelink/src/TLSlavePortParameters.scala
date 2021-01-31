package tilelink

import scala.reflect.{classTag, ClassTag}

case class TLSlavePortParameters(
  slaves:           Vector[TLSlaveParameters],
  channelBeatBytes: TLChannelBeatBytes,
  endSinkId:        Int) {

  /** all possible transaction support by this port. */
  val supports: Set[MasterToSlaveMessage] = slaves
    .flatMap((_: TLSlaveParameters).supports)
    .groupBy((_: MasterToSlaveMessage).getClass())
    .map {
      case (_, v) =>
        v.reduce((_: MasterToSlaveMessage).union(_: MasterToSlaveMessage))
    }
    .toSet

  /** all possible transaction emit by this port. */
  val emits = slaves
    .flatMap((_: TLSlaveParameters).emits)
    .groupBy((_: SlaveToMasterMessage).getClass())
    .map {
      case (_, v) =>
        v.reduce((_: SlaveToMasterMessage).union(_: SlaveToMasterMessage))
    }
    .toSet

  /** the maximum address on this port. */
  val maxAddress: BigInt = slaves.map((_: TLSlaveParameters).maxAddress).max

  /** get maximum transform size on a specific port.
    *
    * @tparam Channel can be [[TLChannelA]], [[TLChannelB]], [[TLChannelC]], [[TLChannelD]], [[TLChannelE]]
    */
  def maxTransferSize[Channel <: TLChannel: ClassTag]: Int =
    slaves.map((_: TLSlaveParameters).maxTransferSize[Channel]).max

  /** Get all parameter `T` from [[emits]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[emits]].
    */
  def supports[T <: TLMessage: ClassTag]: Set[T] =
    supports.filter(classTag[T].runtimeClass.isInstance(_: MasterToSlaveMessage)).asInstanceOf[Set[T]]

  /** Get all parameter `T` from [[supports]]
    *
    * @tparam T the [[TLMessage]] type need to filter from [[supports]].
    */
  def emits[T <: TLMessage: ClassTag]: Set[T] =
    emits.filter(classTag[T].runtimeClass.isInstance(_: SlaveToMasterMessage)).asInstanceOf[Set[T]]
}
