// See LICENSE for license details.
package org.chipsalliance.utils.prci

import diplomacy._
import org.chipsalliance.utils.crossing.ResetCatchAndSync

/**
  * Synchronizes the reset of a diplomatic clock-reset pair to its accompanying clock.
  */
class ResetSynchronizer extends LazyModule {
  val node: ClockAdapterNode = ClockAdapterNode()
  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    node.out.zip(node.in).map {
      case ((o, _), (i, _)) =>
        o.clock := i.clock
        o.reset := ResetCatchAndSync(i.clock, i.reset.asBool)
    }
  }
}

object ResetSynchronizer {
  def apply()(implicit valName: sourcecode.Name): ClockAdapterNode = LazyModule(new ResetSynchronizer()).node
}

/**
  * Instantiates a reset synchronizer on all clock-reset pairs in a clock group.
  */
class ClockGroupResetSynchronizer(implicit valName: sourcecode.Name) extends LazyModule {
  val node: ClockGroupAdapterNode = ClockGroupAdapterNode()
  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    node.out.zip(node.in).map {
      case ((oG, _), (iG, _)) =>
        oG.member.data.zip(iG.member.data).foreach {
          case (o, i) =>
            o.clock := i.clock
            o.reset := ResetCatchAndSync(i.clock, i.reset.asBool)
        }
    }
  }
}

object ClockGroupResetSynchronizer {
  def apply()(implicit valName: sourcecode.Name): ClockGroupAdapterNode = LazyModule(
    new ClockGroupResetSynchronizer()
  ).node
}
