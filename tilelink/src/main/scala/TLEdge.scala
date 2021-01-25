package tilelink

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy.FormatEdge

case class TLEdge(
  clientPortParameters:  TLClientPortParameters,
  managerPortParameters: TLManagerPortParameters,
  sourceInfo:            SourceInfo)
    extends FormatEdge {
  override def formatEdge: String = "TODO"

  val bundleParameters: TLBundleParameters = TLBundleParameters(clientPortParameters, managerPortParameters)
}

class TLEdgeOut(
  clientPortParameters:  TLClientPortParameters,
  managerPortParameters: TLManagerPortParameters,
  sourceInfo:            SourceInfo)
    extends TLEdge(clientPortParameters, managerPortParameters, sourceInfo)

class TLEdgeIn(
  clientPortParameters:  TLClientPortParameters,
  managerPortParameters: TLManagerPortParameters,
  sourceInfo:            SourceInfo)
    extends TLEdge(clientPortParameters, managerPortParameters, sourceInfo)

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
