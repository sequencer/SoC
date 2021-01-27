package tilelink

import org.chipsalliance.utils.addressing.TransferSizes

trait TLTransaction

trait ClientToManagerTransaction extends TLTransaction
trait ManagerToClientTransaction extends TLTransaction
trait ChannelATransaction extends ClientToManagerTransaction
trait ChannelCTransaction extends ClientToManagerTransaction
trait ChannelETransaction extends ClientToManagerTransaction
trait ChannelBTransaction extends ManagerToClientTransaction
trait ChannelDTransaction extends ManagerToClientTransaction

trait HasTransferSizes { this: TLTransaction =>
  val transferSizes: TransferSizes
}

trait HasData { this: TLTransaction with HasTransferSizes =>
  val mayCorrupt: Boolean
}

trait HasDeny { this: TLTransaction =>
  val mayDeny: Boolean
}

trait IsAtomic { this: TLTransaction => }

trait IsArithmetic extends IsAtomic { this: TLTransaction =>
  val MIN:  Boolean
  val MAX:  Boolean
  val MINU: Boolean
  val MAXU: Boolean
  val ADD:  Boolean
}

trait IsLogical extends IsAtomic { this: TLTransaction =>
  val XOR: Boolean
  val OR:  Boolean
  val AND: Boolean
}

trait IsPrefetch { this: TLTransaction =>
  val PrefetchRead:  Boolean
  val PrefetchWrite: Boolean
}

trait HasPermissionTransfer { this: TLTransaction => }

trait HasPruneOrReport extends HasPermissionTransfer { this: TLTransaction =>
  val TtoB: Boolean
  val TtoN: Boolean
  val BtoN: Boolean
  val TtoT: Boolean
  val BtoB: Boolean
  val NtoN: Boolean
}

trait HasCap extends HasPermissionTransfer { this: TLTransaction =>
  val toN: Boolean
  val toB: Boolean
  val toT: Boolean
}

trait HasGrow extends HasPermissionTransfer { this: TLTransaction =>
  val NtoB: Boolean
  val NtoT: Boolean
  val BtoT: Boolean
}

// Get/Atomic

/** ----> [[AccessAckDataD]] */
case class GetA(
  transferSizes: TransferSizes)
    extends ChannelATransaction
    with HasTransferSizes

/** ----> [[AccessAckDataD]] */
case class ArithmeticDataA(
  transferSizes: TransferSizes,
  MIN:           Boolean,
  MAX:           Boolean,
  MINU:          Boolean,
  MAXU:          Boolean,
  ADD:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasData
    with IsArithmetic

/** ----> [[AccessAckDataD]] */
case class LogicalDataA(
  transferSizes: TransferSizes,
  XOR:           Boolean,
  OR:            Boolean,
  AND:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasData
    with IsLogical

/** <---- [[GetA]], [[ArithmeticDataA]], [[LogicalDataA]] */
case class AccessAckDataD(
  transferSizes: TransferSizes,
  mayDeny:       Boolean,
  mayCorrupt:    Boolean)
    extends ChannelDTransaction
    with HasTransferSizes
    with HasData
    with HasDeny

// Put

/** ----> [[AccessAckD]] */
case class PutFullDataA(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasData

/** ----> [[AccessAckD]] */
case class PutPartialDataA(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasData

/** <---- [[PutFullDataA]], [[PutPartialDataA]] */
case class AccessAckD(
  mayDeny: Boolean)
    extends ChannelDTransaction
    with HasDeny

// Intent
/** ----> [[HintAckD]] */
case class IntentA(
  transferSizes: TransferSizes,
  PrefetchRead:  Boolean,
  PrefetchWrite: Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with IsPrefetch

/** <---- [[IntentA]] */
case class HintAckD(
  mayDeny: Boolean)
    extends ChannelDTransaction
    with HasDeny

// Acquire

/** ----> [[GrantD]], [[GrantDataD]] */
case class AcquireBlockA(
  transferSizes: TransferSizes,
  NtoB:          Boolean,
  NtoT:          Boolean,
  BtoT:          Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasGrow

/** ----> [[GrantD]] */
case class AcquirePermA(
  transferSizes: TransferSizes,
  NtoB:          Boolean,
  NtoT:          Boolean,
  BtoT:          Boolean)
    extends ChannelATransaction
    with HasTransferSizes
    with HasGrow

/** <---- [[AcquireBlockA]], [[AcquirePermA]]
  * ----> [[GrantAckE]]
  */
case class GrantD(
  transferSizes: TransferSizes,
  toT:           Boolean,
  toB:           Boolean,
  toN:           Boolean,
  mayDeny:       Boolean)
    extends ChannelDTransaction
    with HasTransferSizes
    with HasCap
    with HasDeny

/** <---- [[AcquireBlockA]]
  * ----> [[GrantAckE]]
  */
case class GrantDataD(
  transferSizes: TransferSizes,
  toT:           Boolean,
  toB:           Boolean,
  toN:           Boolean,
  mayDeny:       Boolean,
  mayCorrupt:    Boolean)
    extends ChannelDTransaction
    with HasTransferSizes
    with HasData
    with HasDeny
    with HasCap

/** <---- [[GrantD]] */
case class GrantAckE() extends ChannelETransaction

// Probe

/** ----> [[ProbeAckC]], [[ProbeAckDataC]] */
case class ProbeBlockB(
  transferSizes: TransferSizes,
  toN:           Boolean,
  toB:           Boolean,
  toT:           Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with HasCap

/** ----> [[ProbeAckC]] */
case class ProbePermB(
  transferSizes: TransferSizes,
  toN:           Boolean,
  toB:           Boolean,
  toT:           Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with HasCap

/** <---- [[ProbeBlockB]], [[ProbePermB]] */
case class ProbeAckC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasPruneOrReport

/** <---- [[ProbeBlockB]] */
case class ProbeAckDataC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean,
  mayCorrupt:    Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasData
    with HasPruneOrReport

// Release

/** ----> [[ReleaseAckD]] */
case class ReleaseC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasPruneOrReport

/** ----> [[ReleaseAckD]] */
case class ReleaseDataC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean,
  mayCorrupt:    Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasData
    with HasPruneOrReport

/** <---- [[ReleaseC]], [[ReleaseDataC]] */
case class ReleaseAckD(
  transferSizes: TransferSizes)
    extends ChannelDTransaction
    with HasTransferSizes

// Get/Atomic in TL-C

/** ----> [[AccessAckDataC]] */
case class GetB(
  transferSizes: TransferSizes)
    extends ChannelBTransaction
    with HasTransferSizes

/** ----> [[AccessAckDataC]] */
case class ArithmeticDataB(
  transferSizes: TransferSizes,
  MIN:           Boolean,
  MAX:           Boolean,
  MINU:          Boolean,
  MAXU:          Boolean,
  ADD:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with HasData
    with IsArithmetic

/** ----> [[AccessAckDataC]] */
case class LogicalDataB(
  transferSizes: TransferSizes,
  XOR:           Boolean,
  OR:            Boolean,
  AND:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with HasData
    with IsLogical

/** <---- [[GetB]], [[ArithmeticDataB]], [[LogicalDataB]]
  * @note cannot deny.
  */
case class AccessAckDataC(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasData

// Put in TL-C

/** ----> [[AccessAckC]] */
case class PutFullDataB(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with HasData

/** ----> [[AccessAckC]] */
case class PutPartialDataB(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelCTransaction
    with HasTransferSizes
    with HasData

/** <---- [[PutFullDataB]], [[PutPartialDataB]]
  * @note cannot deny.
  */
case class AccessAckC() extends ChannelCTransaction

// Intent in TL-C
/** ----> [[HintAckC]] */
case class IntentB(
  transferSizes: TransferSizes,
  PrefetchRead:  Boolean,
  PrefetchWrite: Boolean)
    extends ChannelBTransaction
    with HasTransferSizes
    with IsPrefetch

/** <---- [[IntentB]]
  * @note cannot deny.
  */
case class HintAckC() extends ChannelCTransaction
