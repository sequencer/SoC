package tilelink

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

class TLEdgeIn(
  clientPortParameters:  TLClientPortParameters,
  managerPortParameters: TLManagerPortParameters,
  sourceInfo:            SourceInfo)
    extends TLEdge(clientPortParameters, managerPortParameters, sourceInfo) {
  def AccessAckD(
    size:   UInt,
    source: UInt,
    denied: Bool
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportAccessAckD,
      s"""No clients visible from this edge support AccessAck in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.AccessAck,
      param = 0.U,
      size = size,
      source = source,
      sink = 0.U,
      denied = denied,
      data = 0.U,
      corrupt = false.B
    )
  }
  def AccessAckDataD(
    size:    UInt,
    source:  UInt,
    denied:  Bool,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportAccessAckDataD,
      s"""No clients visible from this edge support AccessAckData in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.AccessAckData,
      param = 0.U,
      size = size,
      source = source,
      sink = 0.U,
      denied = denied,
      data = data,
      corrupt = corrupt
    )

  }
  def HintAckD(
    size:   UInt,
    source: UInt,
    denied: UInt
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportHintAckD,
      s"""No clients visible from this edge support HintAck in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.HintAck,
      param = 0.U,
      size = size,
      source = source,
      sink = 0.U,
      denied = denied,
      data = 0.U,
      corrupt = false.B
    )

  }
  def ProbeBlockB(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportProbeBlockB,
      s"""No clients visible from this edge support ProbeBlock in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
      opcode = TLOpcode.ProbeBlock,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
  }
  def ProbePermB(
    size:    UInt,
    param:   CapParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportProbePermB,
      s"""No clients visible from this edge support ProbePerm in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
      opcode = TLOpcode.ProbePerm,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
  }
  def GrantD(
    size:   UInt,
    param:  CapParam.Type,
    source: UInt,
    sink:   UInt,
    denied: Bool
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportGrantD,
      s"""No clients visible from this edge support Grant in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.Grant,
      param = param.asUInt(),
      size = size,
      source = source,
      sink = sink,
      denied = denied,
      data = 0.U,
      corrupt = false.B
    )
  }
  def GrantDataD(
    size:    UInt,
    param:   CapParam.Type,
    source:  UInt,
    sink:    UInt,
    denied:  Bool,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportGrantD,
      s"""No clients visible from this edge support GrantData in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.GrantData,
      param = param.asUInt(),
      size = size,
      source = source,
      sink = sink,
      denied = denied,
      data = data,
      corrupt = corrupt
    )
  }
  def ReleaseAckD(
    size:   UInt,
    source: UInt
  ): TLChannelD = {
    require(
      clientPortParameters.anySupportReleaseAckD,
      s"""No clients visible from this edge support ReleaseAck in D channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignD(
      opcode = TLOpcode.ReleaseAck,
      param = 0.U,
      size = size,
      source = source,
      sink = 0.U,
      denied = false.B,
      data = 0.U,
      corrupt = false.B
    )
  }
  def GetB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportGetB,
      s"""No clients visible from this edge support Get in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
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
  def PutFullDataB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportPutFullDataB,
      s"""No clients visible from this edge support PutFullData in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
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
  def PutPartialDataB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportPutPartialDataB,
      s"""No clients visible from this edge support PutPartialData in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
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
  def ArithmeticDataB(
    size:    UInt,
    param:   ArithmeticDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportArithmeticDataB,
      s"""No clients visible from this edge support ArithmeticData in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
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
  def LogicalDataB(
    size:    UInt,
    param:   LogicalDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportLogicalDataB,
      s"""No clients visible from this edge support LogicalData in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
      opcode = TLOpcode.LogicalData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = data,
      corrupt = corrupt
    )
  }
  def IntentB(
    size:    UInt,
    param:   IntentParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      clientPortParameters.anySupportIntentB,
      s"""No clients visible from this edge support Intent in B channel,
         |but one of these managers would try to request one: ${managerPortParameters.managers}""".stripMargin
    )
    assignB(
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
}
