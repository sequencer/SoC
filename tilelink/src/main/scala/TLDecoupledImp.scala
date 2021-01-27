package tilelink

import chisel3.internal.sourceinfo.SourceInfo
import diplomacy.{InwardNode, NodeImp, OutwardNode, RenderedEdge}

object TLDecoupledImp
    extends NodeImp[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLDecoupledBundle] {
  def edgeO(pd: TLClientPortParameters, pu: TLManagerPortParameters, sourceInfo: SourceInfo): TLEdgeOut =
    new TLEdgeOut(pd, pu, sourceInfo)

  def bundleO(eo: TLEdgeOut): TLDecoupledBundle = TLBundle.decoupled(eo.bundleParameters)

  def edgeI(pd: TLClientPortParameters, pu: TLManagerPortParameters, sourceInfo: SourceInfo): TLEdgeIn =
    new TLEdgeIn(pd, pu, sourceInfo)

  def bundleI(ei: TLEdgeIn): TLDecoupledBundle = TLBundle.decoupled(ei.bundleParameters)

  def render(ei: TLEdgeIn): RenderedEdge =
    RenderedEdge(
      colour = "#000000",
      label = (ei.managerPortParameters.channelBeatBytes.members.max * 8).toString
    )

  override def mixO(pd: TLClientPortParameters, node: OutwardNode[TLClientPortParameters, TLManagerPortParameters, TLDecoupledBundle]): TLClientPortParameters  =
    pd.copy(clients = pd.clients.map {c => c.copy(nodePath = node +: c.nodePath)})
  override def mixI(pu: TLManagerPortParameters, node: InwardNode[TLClientPortParameters, TLManagerPortParameters, TLDecoupledBundle]): TLManagerPortParameters =
    pu.copy(managers = pu.managers.map { m => m.copy (nodePath = node +: m.nodePath) })

}
