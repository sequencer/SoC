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

/** A TileLink decoupled node which won't modify parameters pass through from it. */
case class TLDecoupledIdentityNode()(implicit valName: sourcecode.Name)
    extends IdentityNode(TLDecoupledImp)
    with TLDecoupledFormatNode

/** A TileLink ephemeral decoupled node to connect two nodes which doesn't not know each other. */
case class TLDecoupledEphemeralNode()(implicit valName: sourcecode.Name) extends EphemeralNode(TLDecoupledImp)

/** A TileLink master decoupled node which can be used as source of SoC connection graph.
  * For example, this node can be used as:
  *   - LSU(Load Store Unit) which can send TileLink message for load and store instructions
  *   - IFU(Instruction Fetch Unit) which can send TileLink message to fetch instructions
  *   - PTW(Page Table Walk) which can send TileLink message to fetch page table
  *   - some custom coprocessor which have its own design purpose
  *
  * @param portParams a sequence of [[TLMasterPortParameters]]
  */
case class TLDecoupledMasterNode(portParams: Seq[TLMasterPortParameters])(implicit valName: sourcecode.Name)
    extends SourceNode(TLDecoupledImp)(portParams)
    with TLDecoupledFormatNode

/** A TileLink slave decoupled node which can be used as sink of SoC connection graph.
  * For example, this node can be used as:
  *   - Memory controller which can integrate with DDR phy by processing [[GetA]], [[PutFullDataA]] message
  *   by responding [[AccessAckDataD]].
  *   - On-chip SRAM which can be used as tightly integrated memory for a CPU.
  *   - On-chip ROM which can store hard-coded data to be used as boot rom or debug rom.
  *   - (Memory Mapped IO) MMIO periphery which can convert memory access to IO behavior.
  */
case class TLDecoupledSlaveNode(portParams: Seq[TLSlavePortParameters])(implicit valName: sourcecode.Name)
    extends SinkNode(TLDecoupledImp)(portParams)
    with TLDecoupledFormatNode

/** A TileLink decoupled node which preserves edge size with modifying parameter to which.
  * If bus devices modified any transactions, for example, source or address,
  * this node should be used to modify parameter as well.
  */
case class TLDecoupledAdapterNode(
  clientFn:  TLMasterPortParameters => TLMasterPortParameters = { s: TLMasterPortParameters => s },
  managerFn: TLSlavePortParameters => TLSlavePortParameters = { s: TLSlavePortParameters => s }
)(
  implicit valName: sourcecode.Name)
    extends AdapterNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

/** This is the special node for multi-bank design.
  *
  * Suppose you had 4 banks of memory controller and wanted to connect those to two different driver crossbars.
  * In that case you can do this:
  * {{{
  *   controller.node :*= jbar.node
  *   jbar.node :*= xbar1.node
  *   jbar.node :*= xbar2.node
  * }}}
  * If the controller has 4 banks, there will be 4 egress ports on both `xbar1` and `xbar2` and they are arbitrated by
  * the `jbar`.
  */
case class TLDecoupledJunctionNode(
  clientFn:  Seq[TLMasterPortParameters] => Seq[TLMasterPortParameters],
  managerFn: Seq[TLSlavePortParameters] => Seq[TLSlavePortParameters]
)(
  implicit valName: sourcecode.Name)
    extends JunctionNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

/** This node can have different size of master and slave, which is often served as CrossBar, Mesh or any other routing
  * devices.
  */
case class TLDecoupledNexusNode(
  clientFn:  Seq[TLMasterPortParameters] => TLMasterPortParameters,
  managerFn: Seq[TLSlavePortParameters] => TLSlavePortParameters
)(
  implicit valName: sourcecode.Name)
    extends NexusNode(TLDecoupledImp)(clientFn, managerFn)
    with TLDecoupledFormatNode

/** This is the node that need you implement your own [[CustomNode.resolveStar]] to resolve master and slave node on your
  * own.
  */
abstract class TLDecoupledCustomNode(implicit valName: sourcecode.Name)
    extends CustomNode(TLDecoupledImp)
    with TLDecoupledFormatNode
