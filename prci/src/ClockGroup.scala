// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import diplomacy._
import org.chipsalliance.utils.dts.FixedClockResource

case class ClockGroupNode(groupName: String)(implicit valName: sourcecode.Name)
    extends MixedNexusNode(ClockGroupImp, ClockImp)(
      dFn = { _ => ClockSourceParameters() },
      uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq) }
    ) {
  override def circuitIdentity: Boolean = outputs.size == 1
}

class ClockGroup(groupName: String) extends LazyModule {
  val node: ClockGroupNode = ClockGroupNode(groupName)

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    val (in, _) = node.in.head
    val (out, _) = node.out.unzip

    require(node.in.size == 1)
    require(in.member.size == out.size)

    in.member.data.zip(out).foreach { case (i, o) => o := i }
  }
}

object ClockGroup {
  def apply()(valName: sourcecode.Name): ClockGroupNode = LazyModule(new ClockGroup(valName.value)).node
}

case class ClockGroupAggregateNode(groupName: String)(implicit valName: sourcecode.Name)
    extends NexusNode(ClockGroupImp)(
      dFn = { _ => ClockGroupSourceParameters() },
      uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq.flatMap(_.members)) }
    ) {
  override def circuitIdentity: Boolean = outputs.size == 1
}

class ClockGroupAggregator(groupName: String) extends LazyModule {
  val node: ClockGroupAggregateNode = ClockGroupAggregateNode(groupName)

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    val (in, _) = node.in.unzip
    val (out, _) = node.out.unzip
    val outputs: Seq[ClockBundle] = out.flatMap(_.member.data)

    require(node.in.size == 1, s"Aggregator for groupName: $groupName had ${node.in.size} inward edges instead of 1")
    require(in.head.member.size == outputs.size)
    in.head.member.data.zip(outputs).foreach { case (i, o) => o := i }
  }
}

object ClockGroupAggregator {
  def apply()(implicit valName: sourcecode.Name): ClockGroupAggregateNode = LazyModule(
    new ClockGroupAggregator(valName.value)
  ).node
}

class SimpleClockGroupSource(numSources: Int = 1) extends LazyModule {
  val node: ClockGroupSourceNode = ClockGroupSourceNode(List.fill(numSources) { ClockGroupSourceParameters() })

  lazy val module: LazyModuleImpLike = new LazyModuleImp(this) {

    val (out, _) = node.out.unzip
    out.map { out: ClockGroupBundle =>
      out.member.data.foreach { o =>
        o.clock := clock; o.reset := reset
      }
    }
  }
}

object SimpleClockGroupSource {
  def apply(num: Int = 1)(implicit valName: sourcecode.Name): ClockGroupSourceNode = LazyModule(
    new SimpleClockGroupSource(num)
  ).node
}

case class FixedClockBroadcastNode(fixedClockOpt: Option[ClockParameters])(implicit valName: sourcecode.Name)
    extends NexusNode(ClockImp)(
      dFn = { seq =>
        fixedClockOpt
          .map(_ => ClockSourceParameters(give = fixedClockOpt))
          .orElse(seq.headOption)
          .getOrElse(ClockSourceParameters())
      },
      uFn = { seq =>
        fixedClockOpt
          .map(_ => ClockSinkParameters(take = fixedClockOpt))
          .orElse(seq.headOption)
          .getOrElse(ClockSinkParameters())
      },
      inputRequiresOutput = false
    ) {
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = Seq(
    fixedClockOpt.map(t => new FixedClockResource(name, t.freqMHz, prefix))
  )
}

class FixedClockBroadcast(fixedClockOpt: Option[ClockParameters]) extends LazyModule {
  val node: FixedClockBroadcastNode = new FixedClockBroadcastNode(fixedClockOpt) {
    override def circuitIdentity: Boolean = outputs.size == 1
  }

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    val (in, _) = node.in.head
    val (out, _) = node.out.unzip
    require(node.in.size == 1, "FixedClockBroadcast can only broadcast a single clock")
    out.foreach { _ := in }
  }
}

object FixedClockBroadcast {
  def apply(fixedClockOpt: Option[ClockParameters])(implicit valName: sourcecode.Name): FixedClockBroadcastNode =
    LazyModule(
      new FixedClockBroadcast(fixedClockOpt)
    ).node
}

case class PRCIClockGroupNode()(implicit valName: sourcecode.Name)
    extends NexusNode(ClockGroupImp)(
      dFn = { _ => ClockGroupSourceParameters() },
      uFn = { _ => ClockGroupSinkParameters("prci", Nil) },
      outputRequiresInput = false
    )
