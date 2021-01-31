package tilelink

import org.chipsalliance.utils.addressing.TransferSizes

object TLMessage {
  private[tilelink] def getMaxTransferSizes[T <: TLMessage](tlMessages: Set[T]): Int =
    try {
      tlMessages
        .filter((_: T).isInstanceOf[HasTransferSizes])
        .map((_: TLMessage).asInstanceOf[HasTransferSizes])
        .map((_: HasTransferSizes).transferSizes.max)
        .max
    } catch {
      // If set is empty, return 0
      case _: UnsupportedOperationException => 0
    }
}

trait TLMessage {

  /** can this support that message? */
  def support(emit: TLMessage): Boolean = if (emit.isInstanceOf[this.type]) true else false

  /** union two same type [[TLMessage]].
    *
    * @note
    * There is a big issue on [[TransferSizes.mincover]] which might expand [[TransferSizes]] to an impossible size.
    */
  def union[Message <: TLMessage](that: Message): this.type =
    throw new RuntimeException(s"not possible to union $this and $that")
}

trait MasterToSlaveMessage extends TLMessage

trait SlaveToMasterMessage extends TLMessage

trait ChannelAMessage extends MasterToSlaveMessage

trait ChannelCMessage extends MasterToSlaveMessage

trait ChannelEMessage extends MasterToSlaveMessage

trait ChannelBMessage extends SlaveToMasterMessage

trait ChannelDMessage extends SlaveToMasterMessage

trait HasTransferSizes extends TLMessage {
  val transferSizes: TransferSizes

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    transferSizes.contains(emit.asInstanceOf[this.type].transferSizes)
}

trait HasData extends TLMessage {
  this: HasTransferSizes =>
  val mayCorrupt: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!mayCorrupt && emit.asInstanceOf[this.type].mayCorrupt)
}

trait HasDeny extends TLMessage {
  val mayDeny: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    !(!mayDeny && emit.asInstanceOf[this.type].mayDeny)
}

trait IsAtomic extends TLMessage

trait IsArithmetic extends IsAtomic {
  val MIN:  Boolean
  val MAX:  Boolean
  val MINU: Boolean
  val MAXU: Boolean
  val ADD:  Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!MIN && emit.asInstanceOf[this.type].MIN) ||
    (!MAX && emit.asInstanceOf[this.type].MAX) ||
    (!MINU && emit.asInstanceOf[this.type].MINU) ||
    (!MAXU && emit.asInstanceOf[this.type].MAXU) ||
    (!ADD && emit.asInstanceOf[this.type].ADD)
}

trait IsLogical extends IsAtomic {
  val XOR: Boolean
  val OR:  Boolean
  val AND: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!XOR && emit.asInstanceOf[this.type].XOR) ||
    (!OR && emit.asInstanceOf[this.type].OR) ||
    (!AND && emit.asInstanceOf[this.type].AND)
}

trait IsPrefetch extends TLMessage {
  val PrefetchRead:  Boolean
  val PrefetchWrite: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!PrefetchRead && emit.asInstanceOf[this.type].PrefetchRead) ||
    (!PrefetchWrite && emit.asInstanceOf[this.type].PrefetchWrite)
}

trait HasPermissionTransfer extends TLMessage

trait HasPruneOrReport extends HasPermissionTransfer {
  val TtoB: Boolean
  val TtoN: Boolean
  val BtoN: Boolean
  val TtoT: Boolean
  val BtoB: Boolean
  val NtoN: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!TtoB && emit.asInstanceOf[this.type].TtoB) ||
    (!TtoN && emit.asInstanceOf[this.type].TtoN) ||
    (!BtoN && emit.asInstanceOf[this.type].BtoN) ||
    (!TtoT && emit.asInstanceOf[this.type].TtoT) ||
    (!BtoB && emit.asInstanceOf[this.type].BtoB) ||
    (!NtoN && emit.asInstanceOf[this.type].NtoN)
}

trait HasCap extends HasPermissionTransfer {
  val toN: Boolean
  val toB: Boolean
  val toT: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!toN && emit.asInstanceOf[this.type].toN) ||
    (!toB && emit.asInstanceOf[this.type].toB) ||
    (!toT && emit.asInstanceOf[this.type].toT)
}

trait HasGrow extends HasPermissionTransfer {
  val NtoB: Boolean
  val NtoT: Boolean
  val BtoT: Boolean

  override def support(emit: TLMessage): Boolean = super.support(emit) &&
    (!NtoB && emit.asInstanceOf[this.type].NtoB) ||
    (!NtoT && emit.asInstanceOf[this.type].NtoT) ||
    (!BtoT && emit.asInstanceOf[this.type].BtoT)
}

// Get/Atomic

/** ----> [[AccessAckDataD]] */
final case class GetA(
  transferSizes: TransferSizes)
    extends ChannelAMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GetA =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckDataD]] */
final case class ArithmeticDataA(
  transferSizes: TransferSizes,
  MIN:           Boolean,
  MAX:           Boolean,
  MINU:          Boolean,
  MAXU:          Boolean,
  ADD:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasData
    with IsArithmetic {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ArithmeticDataA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        MIN | value.MIN,
        MAX | value.MAX,
        MINU | value.MINU,
        MAXU | value.MAXU,
        ADD | value.ADD,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckDataD]] */
final case class LogicalDataA(
  transferSizes: TransferSizes,
  XOR:           Boolean,
  OR:            Boolean,
  AND:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasData
    with IsLogical {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: LogicalDataA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        XOR | value.XOR,
        OR | value.OR,
        AND | value.AND,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[GetA]], [[ArithmeticDataA]], [[LogicalDataA]] */
final case class AccessAckDataD(
  transferSizes: TransferSizes,
  mayDeny:       Boolean,
  mayCorrupt:    Boolean)
    extends ChannelDMessage
    with HasTransferSizes
    with HasData
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AccessAckDataD =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayDeny | value.mayDeny,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Put

/** ----> [[AccessAckD]] */
final case class PutFullDataA(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasData {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: PutFullDataA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckD]] */
final case class PutPartialDataA(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasData {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: PutPartialDataA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[PutFullDataA]], [[PutPartialDataA]] */
final case class AccessAckD(
  mayDeny: Boolean)
    extends ChannelDMessage
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AccessAckD =>
      copy(
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Intent
/** ----> [[HintAckD]] */
final case class IntentA(
  transferSizes: TransferSizes,
  PrefetchRead:  Boolean,
  PrefetchWrite: Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with IsPrefetch {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: IntentA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        PrefetchRead | value.PrefetchRead,
        PrefetchWrite | value.PrefetchWrite
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[IntentA]] */
final case class HintAckD(
  mayDeny: Boolean)
    extends ChannelDMessage
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: HintAckD =>
      copy(
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Acquire

/** ----> [[GrantD]], [[GrantDataD]] */
final case class AcquireBlockA(
  transferSizes: TransferSizes,
  NtoB:          Boolean,
  NtoT:          Boolean,
  BtoT:          Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasGrow {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AcquireBlockA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        NtoB | value.NtoB,
        NtoT | value.NtoT,
        BtoT | value.BtoT
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[GrantD]] */
final case class AcquirePermA(
  transferSizes: TransferSizes,
  NtoB:          Boolean,
  NtoT:          Boolean,
  BtoT:          Boolean)
    extends ChannelAMessage
    with HasTransferSizes
    with HasGrow {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AcquirePermA =>
      copy(
        transferSizes.mincover(value.transferSizes),
        NtoB | value.NtoB,
        NtoT | value.NtoT,
        BtoT | value.BtoT
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[AcquireBlockA]], [[AcquirePermA]]
  * ----> [[GrantAckE]]
  */
final case class GrantD(
  transferSizes: TransferSizes,
  toT:           Boolean,
  toB:           Boolean,
  toN:           Boolean,
  mayDeny:       Boolean)
    extends ChannelDMessage
    with HasTransferSizes
    with HasCap
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GrantD =>
      copy(
        transferSizes.mincover(value.transferSizes),
        toT | value.toT,
        toB | value.toB,
        toN | value.toN,
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[AcquireBlockA]]
  * ----> [[GrantAckE]]
  */
final case class GrantDataD(
  transferSizes: TransferSizes,
  toT:           Boolean,
  toB:           Boolean,
  toN:           Boolean,
  mayDeny:       Boolean,
  mayCorrupt:    Boolean)
    extends ChannelDMessage
    with HasTransferSizes
    with HasData
    with HasDeny
    with HasCap {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GrantDataD =>
      copy(
        transferSizes.mincover(value.transferSizes),
        toT | value.toT,
        toB | value.toB,
        toN | value.toN,
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[GrantD]] */
final case class GrantAckE() extends ChannelEMessage {
  override def union[Message <: TLMessage](that: Message) = that match {
    case _: GrantAckE => this
    case _ => super.union(that)
  }
}

// Probe

/** ----> [[ProbeAckC]], [[ProbeAckDataC]] */
final case class ProbeBlockB(
  transferSizes: TransferSizes,
  toN:           Boolean,
  toB:           Boolean,
  toT:           Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with HasCap {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ProbeBlockB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        toN | value.toN,
        toB | value.toB,
        toT | value.toT
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[ProbeAckC]] */
final case class ProbePermB(
  transferSizes: TransferSizes,
  toN:           Boolean,
  toB:           Boolean,
  toT:           Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with HasCap {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ProbePermB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        toN | value.toN,
        toB | value.toB,
        toT | value.toT
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[ProbeBlockB]], [[ProbePermB]] */
final case class ProbeAckC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasPruneOrReport {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ProbeAckC =>
      copy(
        transferSizes.mincover(value.transferSizes),
        TtoB | value.TtoB,
        TtoN | value.TtoN,
        BtoN | value.BtoN,
        TtoT | value.TtoT,
        BtoB | value.BtoB,
        NtoN | value.NtoN
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[ProbeBlockB]] */
final case class ProbeAckDataC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean,
  mayCorrupt:    Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasData
    with HasPruneOrReport {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ProbeAckDataC =>
      copy(
        transferSizes.mincover(value.transferSizes),
        TtoB | value.TtoB,
        TtoN | value.TtoN,
        BtoN | value.BtoN,
        TtoT | value.TtoT,
        BtoB | value.BtoB,
        NtoN | value.NtoN
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Release

/** ----> [[ReleaseAckD]] */
final case class ReleaseC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasPruneOrReport {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ReleaseC =>
      copy(
        transferSizes.mincover(value.transferSizes),
        TtoB | value.TtoB,
        TtoN | value.TtoN,
        BtoN | value.BtoN,
        TtoT | value.TtoT,
        BtoB | value.BtoB,
        NtoN | value.NtoN
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[ReleaseAckD]] */
final case class ReleaseDataC(
  transferSizes: TransferSizes,
  TtoB:          Boolean,
  TtoN:          Boolean,
  BtoN:          Boolean,
  TtoT:          Boolean,
  BtoB:          Boolean,
  NtoN:          Boolean,
  mayCorrupt:    Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasData
    with HasPruneOrReport {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ReleaseDataC =>
      copy(
        transferSizes.mincover(value.transferSizes),
        TtoB | value.TtoB,
        TtoN | value.TtoN,
        BtoN | value.BtoN,
        TtoT | value.TtoT,
        BtoB | value.BtoB,
        NtoN | value.NtoN,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[ReleaseC]], [[ReleaseDataC]] */
final case class ReleaseAckD(
  transferSizes: TransferSizes)
    extends ChannelDMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ReleaseAckD =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Get/Atomic in TL-C

/** ----> [[AccessAckDataC]] */
final case class GetB(
  transferSizes: TransferSizes)
    extends ChannelBMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GetB =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckDataC]] */
final case class ArithmeticDataB(
  transferSizes: TransferSizes,
  MIN:           Boolean,
  MAX:           Boolean,
  MINU:          Boolean,
  MAXU:          Boolean,
  ADD:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with HasData
    with IsArithmetic {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ArithmeticDataB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        MIN | value.MIN,
        MAX | value.MAX,
        MINU | value.MINU,
        MAXU | value.MAXU,
        ADD | value.ADD,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckDataC]] */
final case class LogicalDataB(
  transferSizes: TransferSizes,
  XOR:           Boolean,
  OR:            Boolean,
  AND:           Boolean,
  mayCorrupt:    Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with HasData
    with IsLogical {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: LogicalDataB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        XOR | value.XOR,
        OR | value.OR,
        AND | value.AND,
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[GetB]], [[ArithmeticDataB]], [[LogicalDataB]]
  *
  * @note cannot deny.
  */
final case class AccessAckDataC(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasData {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AccessAckDataC =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

// Put in TL-C

/** ----> [[AccessAckC]] */
final case class PutFullDataB(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with HasData {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: PutFullDataB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** ----> [[AccessAckC]] */
final case class PutPartialDataB(
  transferSizes: TransferSizes,
  mayCorrupt:    Boolean)
    extends ChannelCMessage
    with HasTransferSizes
    with HasData {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: PutPartialDataB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        mayCorrupt | value.mayCorrupt
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[PutFullDataB]], [[PutPartialDataB]]
  *
  * @note cannot deny.
  */
final case class AccessAckC() extends ChannelCMessage {
  override def union[Message <: TLMessage](that: Message) = that match {
    case _: AccessAckC => this
    case _ => super.union(that)
  }
}

// Intent in TL-C
/** ----> [[HintAckC]] */
final case class IntentB(
  transferSizes: TransferSizes,
  PrefetchRead:  Boolean,
  PrefetchWrite: Boolean)
    extends ChannelBMessage
    with HasTransferSizes
    with IsPrefetch {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: IntentB =>
      copy(
        transferSizes.mincover(value.transferSizes),
        PrefetchRead | value.PrefetchRead,
        PrefetchWrite | value.PrefetchWrite
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
}

/** <---- [[IntentB]]
  *
  * @note cannot deny.
  */
final case class HintAckC() extends ChannelCMessage {
  override def union[Message <: TLMessage](that: Message) = that match {
    case _: HintAckC => this
    case _ => super.union(that)
  }
}
