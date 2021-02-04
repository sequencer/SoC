package tilelink

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util.log2Ceil
import diplomacy.FormatEdge
import org.chipsalliance.utils.addressing.MaskGen
import org.chipsalliance.utils.misc.UIntToOH1

import scala.math.max
import scala.reflect.{classTag, ClassTag}

case class TLEdge(
  masterPortParameters: TLMasterPortParameters,
  slavePortParameters:  TLSlavePortParameters,
  sourceInfo:           SourceInfo)
    extends FormatEdge {
  val bundleParameters: TLBundleParameters = TLBundleParameters(masterPortParameters, slavePortParameters)

  override def formatEdge: String = "TODO"

  /** based on the [[beatBytes]] and [[TLDataChannel.size]], count the number of beat represent in OneHot encode.
    *
    * @note
    * It will generate a OneHot decode circuit and a Mux for [[TLDataChannel]]
    *
    * @todo require the channel is derived from this [[TLEdge]] to forbid not matching problem.
    */
  def numBeats1[Channel <: TLChannel: ClassTag](channel: Channel): UInt = {
    // Channel is not [[TLDataChannel]]: E
    channel match {
      case c: TLDataChannel =>
        if (maxTransferSize[Channel] == 0)
          0.U
        else {
          val decode: UInt = (
            UIntToOH1(
              c.size,
              maxTransferSizeLog2[TLDataChannel]
            ) >> log2Ceil(beatBytes[Channel])
          ).asUInt()
          Mux(hasData(channel), decode, 0.U)
        }
      case _ => 0.U
    }
  }

  def maxTransferSizeLog2[Channel <: TLChannel: ClassTag]: Int =
    log2Ceil(maxTransferSize[Channel])

  def maxTransferSize[Channel <: TLChannel: ClassTag]: Int =
    max(masterPortParameters.maxTransferSize[Channel], slavePortParameters.maxTransferSize[Channel])

  def beatBytes[Channel <: TLChannel: ClassTag]: Int = {
    val runtimeClass: Class[_] = classTag[Channel].runtimeClass
    if (classOf[TLChannelA].isAssignableFrom(runtimeClass))
      slavePortParameters.channelBeatBytes.a
    else if (classOf[TLChannelB].isAssignableFrom(runtimeClass))
      slavePortParameters.channelBeatBytes.b
    else if (classOf[TLChannelC].isAssignableFrom(runtimeClass))
      slavePortParameters.channelBeatBytes.c
    else if (classOf[TLChannelD].isAssignableFrom(runtimeClass))
      slavePortParameters.channelBeatBytes.d
    else if (classOf[TLChannelE].isAssignableFrom(runtimeClass))
      0
    else throw new RuntimeException(s"Cannot detect class type for $runtimeClass.")
  }

  def hasData(x: TLChannel): Bool = {
    staticHasData(x)
      .map((_: Boolean).B)
      .getOrElse(x match {
        // ABCD
        case channel: TLOpcodeChannel => channel.hasData
        // E
        case _ => false.B
      })
  }

  /** Statically optimize the case where hasData is a constant base on parameters. */
  private def staticHasData(bundle: TLChannel): Option[Boolean] = {
    bundle match {
      case _: TLMasterToSlaveChannel =>
        val hasDataMessages: Set[MasterToSlaveMessage] =
          slavePortParameters
            .supports[HasData]
            .asInstanceOf[Set[MasterToSlaveMessage]]
        val hasNoDataMessages: Set[MasterToSlaveMessage] =
          slavePortParameters.supports
            .intersect(hasDataMessages)
        if (hasDataMessages.isEmpty) Some(false) else if (hasNoDataMessages.isEmpty) Some(true) else None
      case _: TLSlaveToMasterChannel =>
        val hasDataMessages: Set[SlaveToMasterMessage] =
          masterPortParameters
            .supports[HasData]
            .asInstanceOf[Set[SlaveToMasterMessage]]
        val hasNoDataMessages: Set[SlaveToMasterMessage] =
          masterPortParameters.supports
            .intersect(hasDataMessages)
        if (hasDataMessages.isEmpty) Some(false) else if (hasNoDataMessages.isEmpty) Some(true) else None
    }
  }

  /** Spec 3.3 */
  protected def assignA(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    val a: TLChannelA = Wire(new TLChannelA(bundleParameters.a))
    a.opcode := opcode
    a.param := param
    a.size := size
    a.source := source
    a.address := address
    a.mask := mask
    a.data := data
    a.corrupt := corrupt
    a
  }

  /** Spec 3.4 */
  protected def assignB(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    val b: TLChannelB = Wire(new TLChannelB(bundleParameters.b.get))
    b.opcode := opcode
    b.param := param
    b.size := size
    b.source := source
    b.address := address
    b.mask := mask
    b.data := data
    b.corrupt := corrupt
    b
  }

  /** Spec 3.5 */
  protected def assignC(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    val c: TLChannelC = Wire(new TLChannelC(bundleParameters.c.get))
    c.opcode := opcode
    c.param := param
    c.size := size
    c.source := source
    c.address := address
    c.data := data
    c.corrupt := corrupt
    c
  }

  /** Spec 3.6 */
  protected def assignD(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    sink:    UInt,
    denied:  UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    val d: TLChannelD = Wire(new TLChannelD(bundleParameters.d))
    d.opcode := opcode
    d.param := param
    d.size := size
    d.source := source
    d.sink := sink
    d.denied := denied
    d.data := data
    d.corrupt := corrupt
    d
  }

  /** Spec 3.7 */
  protected def assignE(
    sink: UInt
  ): TLChannelE = {
    val e: TLChannelE = Wire(new TLChannelE(bundleParameters.e.get))
    e.sink := sink
    e
  }
}

object TLOpcode {
  val Get:            UInt = 4.U
  val AccessAckData:  UInt = 1.U
  val PutFullData:    UInt = 0.U
  val PutPartialData: UInt = 1.U
  val AccessAck:      UInt = 0.U
  val ArithmeticData: UInt = 2.U
  val LogicalData:    UInt = 3.U
  val Intent:         UInt = 5.U
  val HintAck:        UInt = 2.U
  val AcquireBlock:   UInt = 6.U
  val AcquirePerm:    UInt = 7.U
  val Grant:          UInt = 4.U
  val GrantData:      UInt = 5.U
  val ProbeBlock:     UInt = 6.U
  val ProbePerm:      UInt = 7.U
  val ProbeAck:       UInt = 4.U
  val ProbeAckData:   UInt = 5.U
  val Release:        UInt = 6.U
  val ReleaseData:    UInt = 7.U
  val ReleaseAck:     UInt = 6.U
}

/** TileLink Spec 1.8.1
  * Table 23
  */
object ArithmeticDataParam extends ChiselEnum {
  val MIN:  ArithmeticDataParam.Type = Value(0.U)
  val MAX:  ArithmeticDataParam.Type = Value(1.U)
  val MINU: ArithmeticDataParam.Type = Value(2.U)
  val MAXU: ArithmeticDataParam.Type = Value(3.U)
  val ADD:  ArithmeticDataParam.Type = Value(4.U)
}

/** TileLink Spec 1.8.1
  * Table 25
  */
object LogicalDataParam extends ChiselEnum {
  val XOR:  LogicalDataParam.Type = Value(0.U)
  val OR:   LogicalDataParam.Type = Value(1.U)
  val AND:  LogicalDataParam.Type = Value(2.U)
  val SWAP: LogicalDataParam.Type = Value(3.U)
}

object IntentParam extends ChiselEnum {
  val PrefetchRead:  IntentParam.Type = Value(0.U)
  val PrefetchWrite: IntentParam.Type = Value(1.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Cap
  */
object CapParam extends ChiselEnum {
  val toT: CapParam.Type = Value(0.U)
  val toB: CapParam.Type = Value(1.U)
  val toN: CapParam.Type = Value(2.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Grow
  */
object GrowParam extends ChiselEnum {
  val NtoB: GrowParam.Type = Value(0.U)
  val NtoT: GrowParam.Type = Value(1.U)
  val BtoT: GrowParam.Type = Value(2.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Prune & Report
  * param can be PruneParam or ReportParam, since it's tricky to implement a union type in Scala 2
  * these should be refactored into one function after Chisel supports Scala 3 in the future.
  */
object PruneReportParam extends ChiselEnum {
  val TtoB: PruneReportParam.Type = Value(0.U)
  val TtoN: PruneReportParam.Type = Value(1.U)
  val BtoN: PruneReportParam.Type = Value(2.U)
  val TtoT: PruneReportParam.Type = Value(3.U)
  val BtoB: PruneReportParam.Type = Value(4.U)
  val NtoN: PruneReportParam.Type = Value(5.U)
}
