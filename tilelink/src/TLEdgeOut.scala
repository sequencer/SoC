package tilelink

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

class TLEdgeOut(
  clientPortParameters:  TLMasterPortParameters,
  managerPortParameters: TLSlavePortParameters,
  sourceInfo:            SourceInfo)
    extends TLEdge(clientPortParameters, managerPortParameters, sourceInfo) {

  /** Spec 7.2.1 */
  def GetA(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA = {
    require(
      managerPortParameters.supports[GetA].nonEmpty,
      s"""No managers visible from this edge support Get in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.Get,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 7.2.2 */
  def PutFullDataA(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.supports[PutFullDataA].nonEmpty,
      s"""No managers visible from this edge support PutFullData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.PutFullData,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 7.2.3 */
  def PutPartialDataA(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.supports[PutPartialDataA].nonEmpty,
      s"""No managers visible from this edge support PutPartialData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.PutPartialData,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 8.2.1 */
  def ArithmeticDataA(
    size:    UInt,
    param:   ArithmeticDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.supports[ArithmeticDataA].nonEmpty,
      s"""No managers visible from this edge support ArithmeticData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.ArithmeticData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 8.2.2 */
  def LogicalDataA(
    size:    UInt,
    param:   ArithmeticDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.supports[LogicalDataA].nonEmpty,
      s"""No managers visible from this edge support LogicalData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.ArithmeticData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 8.2.3 */
  def IntentA(
    size:    UInt,
    param:   IntentParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.supports[IntentA].nonEmpty,
      s"""No managers visible from this edge support Intent in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.Intent,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 9.3.1 */
  def AcquireBlockA(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA = {
    require(
      managerPortParameters.supports[AcquireBlockA].nonEmpty,
      s"""No managers visible from this edge support AcquireBlock in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.AcquireBlock,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 9.3.2 */
  def AcquirePermA(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA = {
    require(
      managerPortParameters.supports[AcquirePermA].nonEmpty,
      s"""No managers visible from this edge support AcquirePerm in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignA(
      opcode = TLOpcode.AcquirePerm,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 9.3.5 */
  def ProbeAckC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.supports[ProbeAckC].nonEmpty,
      s"""No managers visible from this edge support ProbeAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.ProbeAck,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 9.3.6 */
  def ProbeAckDataC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.supports[ProbeAckDataC].nonEmpty,
      s"""No managers visible from this edge support ProbeAckData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.ProbeAckData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 9.3.9 */
  def GrantAckE(
    sink: UInt
  ): TLChannelE = {
    require(
      managerPortParameters.supports[GrantAckE].nonEmpty,
      s"""No managers visible from this edge support GrantAck in E channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignE(
      sink = sink
    )
  }

  /** Spec 9.3.10 */
  def ReleaseC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.supports[ProbeAckC].nonEmpty,
      s"""No managers visible from this edge support Release in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.Release,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 9.3.11 */
  def ReleaseDataC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.supports[ReleaseDataC].nonEmpty,
      s"""No managers visible from this edge support ReleaseData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.ReleaseData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 9.5.4 */
  def AccessAckC(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.supports[AccessAckC].nonEmpty,
      s"""No managers visible from this edge support AccessAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.AccessAck,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
  }

  /** Spec 9.5.5 */
  def AccessAckDataC(
    size:    UInt,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.supports[AccessAckDataC].nonEmpty,
      s"""No managers visible from this edge support AccessAckData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.AccessAckData,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      data = data,
      corrupt = corrupt
    )
  }

  /** Spec 9.5.9 */
  def HintAckC(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.supports[HintAckC].nonEmpty,
      s"""No managers visible from this edge support HintAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.masters}""".stripMargin
    )
    assignC(
      opcode = TLOpcode.HintAck,
      param = 0.U,
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
  }
}
