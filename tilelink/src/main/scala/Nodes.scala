package tilelink

import diplomacy.{
  AdapterNode,
  CustomNode,
  EphemeralNode,
  FormatNode,
  IdentityNode,
  JunctionNode,
  NexusNode,
  SinkNode,
  SourceNode
}

trait TLDecoupledFormatNode extends FormatNode[TLEdgeIn, TLEdgeOut]

case class TLDecoupledIdentityNode()(implicit valName: sourcecode.Name)
    extends IdentityNode(TLDecoupledImp)
    with TLDecoupledFormatNode

case class TLDecoupledEphemeralNode()(implicit valName: sourcecode.Name) extends EphemeralNode(TLDecoupledImp)

case class TLDecoupledClientNode(portParams: Seq[TLClientPortParameters])(implicit valName: sourcecode.Name)
    extends SourceNode(TLDecoupledImp)(portParams)
    with TLDecoupledFormatNode

case class TLDecoupledManagerNode(portParams: Seq[TLManagerPortParameters])(implicit valName: sourcecode.Name)
    extends SinkNode(TLDecoupledImp)(portParams)
    with TLDecoupledFormatNode

case class TLDecoupledAdapterNode(
  clientFn:  TLClientPortParameters => TLClientPortParameters = { s => s },
  managerFn: TLManagerPortParameters => TLManagerPortParameters = { s => s }
)(
  implicit
  valName: sourcecode.Name)
    extends AdapterNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

case class TLDecoupledJunctionNode(
  clientFn:  Seq[TLClientPortParameters] => Seq[TLClientPortParameters],
  managerFn: Seq[TLManagerPortParameters] => Seq[TLManagerPortParameters]
)(
  implicit valName: sourcecode.Name)
    extends JunctionNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

case class TLDecoupledNexusNode(
  clientFn:  Seq[TLClientPortParameters] => TLClientPortParameters,
  managerFn: Seq[TLManagerPortParameters] => TLManagerPortParameters
)(
  implicit valName: sourcecode.Name)
    extends NexusNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

abstract class TLDecoupledCustomNode(implicit valName: sourcecode.Name)
    extends CustomNode(TLDecoupledImp)
    with TLDecoupledFormatNode
