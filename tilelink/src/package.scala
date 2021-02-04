import diplomacy.{InwardNodeHandle, NodeHandle, OutwardNodeHandle}

/** This package is the diplomacy-based TileLink protocol plugin.
  * Most of ideas were borrowed for Rocket-Chip, but the implementation is strictly correspond to TileLink Spec 1.8.1.
  * Unlike the version inside RocketChip, it has more strict design for a safe hardware constructing experience.
  * For each protocol related code, ScalaDoc should annotate the correspond page in the spec to make designer easy to
  * understand implementation vs specification.
  *
  * @note
  * There still exist a issue on double checking emit and support for each usage `xxx.support` `xxx.emit`,
  * They are not the final negotiation result of diplomacy, still exist a possibility that master not emit while
  * client can support. In this case, if check with `xxx.support` may generate redundant circuit, which should not be
  * allowed for performance reason.
  */
package object tilelink {

  /** Proving a left-hand-side [[NodeHandle]] for the binding operator.
    * It's the [[NodeHandle]] for a node served as slave in TileLink.
    */
  type TLDecoupledSlaveNodeHandle = InwardNodeHandle[
    TLMasterPortParameters,
    TLSlavePortParameters,
    TLEdgeIn,
    TLDecoupledBundle
  ]

  /** Proving a right-hand-side [[NodeHandle]] for the binding operators.
    * It's the [[NodeHandle]] for a node served as master in TileLink.
    */
  type TLDecoupledMasterNodeHandle = OutwardNodeHandle[
    TLMasterPortParameters,
    TLSlavePortParameters,
    TLEdgeOut,
    TLDecoupledBundle
  ]

  /** Proving a dual [[NodeHandle]] for the binding operators.
    * This Node can serve as master or slave.
    */
  type TLDecoupledNodeHandle = NodeHandle[
    TLMasterPortParameters,
    TLSlavePortParameters,
    TLEdgeIn,
    TLDecoupledBundle,
    TLMasterPortParameters,
    TLSlavePortParameters,
    TLEdgeOut,
    TLDecoupledBundle
  ]

}
