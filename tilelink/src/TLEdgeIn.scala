package tilelink

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

class TLEdgeIn(
  masterPortParameters: TLMasterPortParameters,
  slavePortParameters:  TLSlavePortParameters,
  sourceInfo:           SourceInfo)
    extends TLEdge(masterPortParameters, slavePortParameters, sourceInfo) {

  /** Spec 7.2.4 */
  def AccessAckD(
    size:   UInt,
    source: UInt,
    denied: Bool
  ): TLChannelD = {
    require(
      masterPortParameters.supports[AccessAckD].nonEmpty,
      s"""No clients visible from this edge support AccessAck in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 7.2.5 */
  def AccessAckDataD(
    size:    UInt,
    source:  UInt,
    denied:  Bool,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    require(
      masterPortParameters.supports[AccessAckD].nonEmpty,
      s"""No clients visible from this edge support AccessAckData in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 8.2.4 */
  def HintAckD(
    size:   UInt,
    source: UInt,
    denied: UInt
  ): TLChannelD = {
    require(
      masterPortParameters.supports[HintAckD].nonEmpty,
      s"""No clients visible from this edge support HintAck in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.3.3 */
  def ProbeBlockB(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      masterPortParameters.supports[ProbeBlockB].nonEmpty,
      s"""No clients visible from this edge support ProbeBlock in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.3.2 */
  def ProbePermB(
    size:    UInt,
    param:   CapParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      masterPortParameters.supports[ProbePermB].nonEmpty,
      s"""No clients visible from this edge support ProbePerm in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.3.7 */
  def GrantD(
    size:   UInt,
    param:  CapParam.Type,
    source: UInt,
    sink:   UInt,
    denied: Bool
  ): TLChannelD = {
    require(
      masterPortParameters.supports[GrantD].nonEmpty,
      s"""No clients visible from this edge support Grant in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.3.8 */
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
      masterPortParameters.supports[GrantD].nonEmpty,
      s"""No clients visible from this edge support GrantData in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.3.12 */
  def ReleaseAckD(
    size:   UInt,
    source: UInt
  ): TLChannelD = {
    require(
      masterPortParameters.supports[ReleaseAckD].nonEmpty,
      s"""No clients visible from this edge support ReleaseAck in D channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.1 */
  def GetB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB = {
    require(
      masterPortParameters.supports[GetB].nonEmpty,
      s"""No clients visible from this edge support Get in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.2 */
  def PutFullDataB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      masterPortParameters.supports[PutFullDataB].nonEmpty,
      s"""No clients visible from this edge support PutFullData in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.3 */
  def PutPartialDataB(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    require(
      masterPortParameters.supports[PutPartialDataB].nonEmpty,
      s"""No clients visible from this edge support PutPartialData in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.6 */
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
      masterPortParameters.supports[ArithmeticDataB].nonEmpty,
      s"""No clients visible from this edge support ArithmeticData in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.7 */
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
      masterPortParameters.supports[LogicalDataB].nonEmpty,
      s"""No clients visible from this edge support LogicalData in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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

  /** Spec 9.5.8 */
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
      masterPortParameters.supports[IntentB].nonEmpty,
      s"""No clients visible from this edge support Intent in B channel,
         |but one of these managers would try to request one: ${slavePortParameters.slaves}""".stripMargin
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
