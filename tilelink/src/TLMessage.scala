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
  def support(emit: TLMessage): Boolean

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

  override def support(emit: TLMessage): Boolean =
    transferSizes.contains(emit.asInstanceOf[this.type].transferSizes)
}

trait HasData extends TLMessage {
  this: HasTransferSizes =>
  val mayCorrupt: Boolean

  override def support(emit: TLMessage): Boolean =
    !(!mayCorrupt && emit.asInstanceOf[this.type].mayCorrupt)
}

trait HasDeny extends TLMessage {
  val mayDeny: Boolean

  override def support(emit: TLMessage): Boolean =
    !(!mayDeny && emit.asInstanceOf[this.type].mayDeny)
}

trait IsAtomic extends TLMessage

trait IsArithmetic extends IsAtomic {
  val MIN:  Boolean
  val MAX:  Boolean
  val MINU: Boolean
  val MAXU: Boolean
  val ADD:  Boolean

  override def support(emit: TLMessage): Boolean =
    !((!MIN && emit.asInstanceOf[this.type].MIN) ||
      (!MAX && emit.asInstanceOf[this.type].MAX) ||
      (!MINU && emit.asInstanceOf[this.type].MINU) ||
      (!MAXU && emit.asInstanceOf[this.type].MAXU) ||
      (!ADD && emit.asInstanceOf[this.type].ADD))
}

trait IsLogical extends IsAtomic {
  val XOR: Boolean
  val OR:  Boolean
  val AND: Boolean

  override def support(emit: TLMessage): Boolean =
    !((!XOR && emit.asInstanceOf[this.type].XOR) ||
      (!OR && emit.asInstanceOf[this.type].OR) ||
      (!AND && emit.asInstanceOf[this.type].AND))
}

trait IsPrefetch extends TLMessage {
  val PrefetchRead:  Boolean
  val PrefetchWrite: Boolean

  override def support(emit: TLMessage): Boolean =
    !((!PrefetchRead && emit.asInstanceOf[this.type].PrefetchRead) ||
      (!PrefetchWrite && emit.asInstanceOf[this.type].PrefetchWrite))
}

trait HasPermissionTransfer extends TLMessage

trait HasPruneOrReport extends HasPermissionTransfer {
  val TtoB: Boolean
  val TtoN: Boolean
  val BtoN: Boolean
  val TtoT: Boolean
  val BtoB: Boolean
  val NtoN: Boolean

  override def support(emit: TLMessage): Boolean =
    !((!TtoB && emit.asInstanceOf[this.type].TtoB) ||
      (!TtoN && emit.asInstanceOf[this.type].TtoN) ||
      (!BtoN && emit.asInstanceOf[this.type].BtoN) ||
      (!TtoT && emit.asInstanceOf[this.type].TtoT) ||
      (!BtoB && emit.asInstanceOf[this.type].BtoB) ||
      (!NtoN && emit.asInstanceOf[this.type].NtoN))
}

trait HasCap extends HasPermissionTransfer {
  val toN: Boolean
  val toB: Boolean
  val toT: Boolean

  override def support(emit: TLMessage): Boolean =
    !((!toN && emit.asInstanceOf[this.type].toN) ||
      (!toB && emit.asInstanceOf[this.type].toB) ||
      (!toT && emit.asInstanceOf[this.type].toT))
}

trait HasGrow extends HasPermissionTransfer {
  val NtoB: Boolean
  val NtoT: Boolean
  val BtoT: Boolean

  /** @todo is this support really good? a master not support [[NtoB]] [[BtoT]] can always grantT to slave. */
  override def support(emit: TLMessage): Boolean =
    !((!NtoB && emit.asInstanceOf[this.type].NtoB) ||
      (!NtoT && emit.asInstanceOf[this.type].NtoT) ||
      (!BtoT && emit.asInstanceOf[this.type].BtoT))
}

// Get/Atomic

/** ----> [[AccessAckDataD]] */
final case class GetA(
  transferSizes: TransferSizes = TransferSizes(0, 4096))
    extends ChannelAMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GetA =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[GetA] &&
    super[HasTransferSizes].support(emit)
}

/** ----> [[AccessAckDataD]] */
final case class ArithmeticDataA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  MIN:           Boolean = true,
  MAX:           Boolean = true,
  MINU:          Boolean = true,
  MAXU:          Boolean = true,
  ADD:           Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ArithmeticDataA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[IsArithmetic].support(emit)
}

/** ----> [[AccessAckDataD]] */
final case class LogicalDataA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  XOR:           Boolean = true,
  OR:            Boolean = true,
  AND:           Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[LogicalDataA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[IsLogical].support(emit)
}

/** <---- [[GetA]], [[ArithmeticDataA]], [[LogicalDataA]] */
final case class AccessAckDataD(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayDeny:       Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[AccessAckDataD] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[HasDeny].support(emit)
}

// Put

/** ----> [[AccessAckD]] */
final case class PutFullDataA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[PutFullDataA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit)
}

/** ----> [[AccessAckD]] */
final case class PutPartialDataA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayCorrupt:    Boolean = true)
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

  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[PutPartialDataA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit)
}

/** <---- [[PutFullDataA]], [[PutPartialDataA]] */
final case class AccessAckD(
  mayDeny: Boolean = true)
    extends ChannelDMessage
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: AccessAckD =>
      copy(
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[AccessAckD] &&
    super[HasDeny].support(emit)
}

// Intent
/** ----> [[HintAckD]] */
final case class IntentA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  PrefetchRead:  Boolean = true,
  PrefetchWrite: Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[IntentA] &&
    super[HasTransferSizes].support(emit) &&
    super[IsPrefetch].support(emit)
}

/** <---- [[IntentA]] */
final case class HintAckD(
  mayDeny: Boolean = true)
    extends ChannelDMessage
    with HasDeny {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: HintAckD =>
      copy(
        mayDeny | value.mayDeny
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[HintAckD] &&
    super[HasDeny].support(emit)
}

// Acquire

/** ----> [[GrantD]], [[GrantDataD]] */
final case class AcquireBlockA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  NtoB:          Boolean = true,
  NtoT:          Boolean = true,
  BtoT:          Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[AcquireBlockA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasGrow].support(emit)
}

/** ----> [[GrantD]] */
final case class AcquirePermA(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  NtoB:          Boolean = true,
  NtoT:          Boolean = true,
  BtoT:          Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[AcquirePermA] &&
    super[HasTransferSizes].support(emit) &&
    super[HasGrow].support(emit)
}

/** <---- [[AcquireBlockA]], [[AcquirePermA]]
  * ----> [[GrantAckE]]
  */
final case class GrantD(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  toT:           Boolean = true,
  toB:           Boolean = true,
  toN:           Boolean = true,
  mayDeny:       Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[GrantD] &&
    super[HasTransferSizes].support(emit) &&
    super[HasCap].support(emit) &&
    super[HasDeny].support(emit)
}

/** <---- [[AcquireBlockA]]
  * ----> [[GrantAckE]]
  */
final case class GrantDataD(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  toT:           Boolean = true,
  toB:           Boolean = true,
  toN:           Boolean = true,
  mayDeny:       Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[GrantDataD] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[HasDeny].support(emit) &&
    super[HasCap].support(emit)
}

/** <---- [[GrantD]] */
final case class GrantAckE() extends ChannelEMessage {
  override def union[Message <: TLMessage](that: Message) = that match {
    case _: GrantAckE => this
    case _ => super.union(that)
  }
  def support(emit: TLMessage): Boolean = emit.isInstanceOf[GrantAckE]
}

// Probe

/** ----> [[ProbeAckC]], [[ProbeAckDataC]] */
final case class ProbeBlockB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  toN:           Boolean = true,
  toB:           Boolean = true,
  toT:           Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ProbeBlockB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasCap].support(emit)
}

/** ----> [[ProbeAckC]] */
final case class ProbePermB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  toN:           Boolean = true,
  toB:           Boolean = true,
  toT:           Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ProbePermB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasCap].support(emit)
}

/** <---- [[ProbeBlockB]], [[ProbePermB]] */
final case class ProbeAckC(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  TtoB:          Boolean = true,
  TtoN:          Boolean = true,
  BtoN:          Boolean = true,
  TtoT:          Boolean = true,
  BtoB:          Boolean = true,
  NtoN:          Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ProbeAckC] &&
    super[HasTransferSizes].support(emit) &&
    super[HasPruneOrReport].support(emit)
}

/** <---- [[ProbeBlockB]] */
final case class ProbeAckDataC(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  TtoB:          Boolean = true,
  TtoN:          Boolean = true,
  BtoN:          Boolean = true,
  TtoT:          Boolean = true,
  BtoB:          Boolean = true,
  NtoN:          Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ProbeAckDataC] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[HasPruneOrReport].support(emit)
}

// Release

/** ----> [[ReleaseAckD]] */
final case class ReleaseC(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  TtoB:          Boolean = true,
  TtoN:          Boolean = true,
  BtoN:          Boolean = true,
  TtoT:          Boolean = true,
  BtoB:          Boolean = true,
  NtoN:          Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ReleaseC] &&
    super[HasTransferSizes].support(emit) &&
    super[HasPruneOrReport].support(emit)
}

/** ----> [[ReleaseAckD]] */
final case class ReleaseDataC(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  TtoB:          Boolean = true,
  TtoN:          Boolean = true,
  BtoN:          Boolean = true,
  TtoT:          Boolean = true,
  BtoB:          Boolean = true,
  NtoN:          Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ReleaseDataC] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[HasPruneOrReport].support(emit)
}

/** <---- [[ReleaseC]], [[ReleaseDataC]] */
final case class ReleaseAckD(
  transferSizes: TransferSizes = TransferSizes(0, 4096))
    extends ChannelDMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: ReleaseAckD =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ReleaseAckD] &&
    super[HasTransferSizes].support(emit)
}

// Get/Atomic in TL-C

/** ----> [[AccessAckDataC]] */
final case class GetB(
  transferSizes: TransferSizes = TransferSizes(0, 4096))
    extends ChannelBMessage
    with HasTransferSizes {
  override def union[Message <: TLMessage](that: Message) = that match {
    case value: GetB =>
      copy(
        transferSizes.mincover(value.transferSizes)
      ).asInstanceOf[this.type]
    case _ => super.union(that)
  }
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[GetB] &&
    super[HasTransferSizes].support(emit)
}

/** ----> [[AccessAckDataC]] */
final case class ArithmeticDataB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  MIN:           Boolean = true,
  MAX:           Boolean = true,
  MINU:          Boolean = true,
  MAXU:          Boolean = true,
  ADD:           Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[ArithmeticDataB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[IsArithmetic].support(emit)
}

/** ----> [[AccessAckDataC]] */
final case class LogicalDataB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  XOR:           Boolean = true,
  OR:            Boolean = true,
  AND:           Boolean = true,
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[LogicalDataB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit) &&
    super[IsLogical].support(emit)
}

/** <---- [[GetB]], [[ArithmeticDataB]], [[LogicalDataB]]
  *
  * @note cannot deny.
  */
final case class AccessAckDataC(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[LogicalDataB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit)
}

// Put in TL-C

/** ----> [[AccessAckC]] */
final case class PutFullDataB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[PutFullDataB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit)
}

/** ----> [[AccessAckC]] */
final case class PutPartialDataB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  mayCorrupt:    Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[PutPartialDataB] &&
    super[HasTransferSizes].support(emit) &&
    super[HasData].support(emit)
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
  def support(emit: TLMessage): Boolean = emit.isInstanceOf[AccessAckC]
}

// Intent in TL-C
/** ----> [[HintAckC]] */
final case class IntentB(
  transferSizes: TransferSizes = TransferSizes(0, 4096),
  PrefetchRead:  Boolean = true,
  PrefetchWrite: Boolean = true)
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
  override def support(emit: TLMessage): Boolean = emit.isInstanceOf[IntentB] &&
    super[HasTransferSizes].support(emit) &&
    super[IsPrefetch].support(emit)
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
  def support(emit: TLMessage): Boolean = emit.isInstanceOf[HintAckC]
}
