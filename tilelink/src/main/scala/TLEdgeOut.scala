package tilelink

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

class TLEdgeOut(
  clientPortParameters:  TLClientPortParameters,
  managerPortParameters: TLManagerPortParameters,
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
      managerPortParameters.anySupportGetA,
      s"""No managers visible from this edge support Get in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
      managerPortParameters.anySupportPutFullDataA,
      s"""No managers visible from this edge support PutFullData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def PutPartialDataA(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    require(
      managerPortParameters.anySupportPutPartialDataA,
      s"""No managers visible from this edge support PutPartialData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
      managerPortParameters.anySupportArithmeticDataA,
      s"""No managers visible from this edge support ArithmeticData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
      managerPortParameters.anySupportLogicalDataA,
      s"""No managers visible from this edge support LogicalData in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
      managerPortParameters.anySupportIntentA,
      s"""No managers visible from this edge support Intent in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def AcquireBlockA(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA = {
    require(
      managerPortParameters.anySupportAcquireBlockA,
      s"""No managers visible from this edge support AcquireBlock in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def AcquirePermA(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA = {
    require(
      managerPortParameters.anySupportAcquirePermA,
      s"""No managers visible from this edge support AcquirePerm in A channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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

  def ProbeAckC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportProbeAckC,
      s"""No managers visible from this edge support ProbeAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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

  def ProbeAckDataC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportProbeAckDataC,
      s"""No managers visible from this edge support ProbeAckData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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

  def GrantAckE(
    sink: UInt
  ): TLChannelE = {
    require(
      managerPortParameters.anySupportGrantAckE,
      s"""No managers visible from this edge support GrantAck in E channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
    )
    assignE(
      sink = sink
    )
  }
  def ReleaseC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportProbeAckC,
      s"""No managers visible from this edge support Release in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def ReleaseDataC(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportReleaseDataC,
      s"""No managers visible from this edge support ReleaseData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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

  def AccessAckC(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportAccessAckC,
      s"""No managers visible from this edge support AccessAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def AccessAckDataC(
    size:    UInt,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportAccessAckDataC,
      s"""No managers visible from this edge support AccessAckData in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
  def HintAckC(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    require(
      managerPortParameters.anySupportHintAckC,
      s"""No managers visible from this edge support HintAck in C channel,
         |but one of these clients would try to request one: ${clientPortParameters.clients}""".stripMargin
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
