package tilelink

import chisel3.internal.sourceinfo.SourceInfo
import diplomacy.{InwardNode, NodeImp, OutwardNode, RenderedEdge}

object TLDecoupledImp
    extends NodeImp[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLEdgeIn, TLDecoupledBundle] {
  def edgeO(pd: TLMasterPortParameters, pu: TLSlavePortParameters, sourceInfo: SourceInfo): TLEdgeOut =
    new TLEdgeOut(pd, pu, sourceInfo)

  def bundleO(eo: TLEdgeOut): TLDecoupledBundle = TLBundle.decoupled(eo.bundleParameters)

  def edgeI(pd: TLMasterPortParameters, pu: TLSlavePortParameters, sourceInfo: SourceInfo): TLEdgeIn =
    new TLEdgeIn(pd, pu, sourceInfo)

  def bundleI(ei: TLEdgeIn): TLDecoupledBundle = TLBundle.decoupled(ei.bundleParameters)

  def render(ei: TLEdgeIn): RenderedEdge =
    RenderedEdge(
      colour = "#000000",
      label = (ei.slavePortParameters.channelBeatBytes.members.max * 8).toString
    )

  override def mixO(
    pd:   TLMasterPortParameters,
    node: OutwardNode[TLMasterPortParameters, TLSlavePortParameters, TLDecoupledBundle]
  ): TLMasterPortParameters =
    pd.copy(masters = pd.masters.map { c: TLMasterParameters => c.copy(nodePath = node +: c.nodePath) })
  override def mixI(
    pu:   TLSlavePortParameters,
    node: InwardNode[TLMasterPortParameters, TLSlavePortParameters, TLDecoupledBundle]
  ): TLSlavePortParameters =
    pu.copy(slaves = pu.slaves.map { m: TLSlaveParameters => m.copy(nodePath = node +: m.nodePath) })

}
