package tilelink

import chisel3.internal.sourceinfo.SourceInfo
import diplomacy.{NodeImp, RenderedEdge}

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
      colour = "#000000" /* black */,
      label = (ei.managerPortParameters.channelBeatBytes.members.flatten.max * 8).toString
    )
}
