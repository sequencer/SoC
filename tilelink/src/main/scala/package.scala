import diplomacy.{InwardNodeHandle, NodeHandle, OutwardNodeHandle}

package object tilelink {
  type TLDecoupledInwardNode =
    InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLDecoupledBundle]
  type TLDecoupledOutwardNode =
    OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLDecoupledBundle]
  type TLDecoupledNode = NodeHandle[
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeIn,
    TLDecoupledBundle,
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLDecoupledBundle
  ]

}
