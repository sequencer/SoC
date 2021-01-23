// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import chisel3._
import chisel3.experimental.IO
import diplomacy.config.Parameters
import diplomacy._

/** Used to parameterize the creation of simple clock group drivers */
case class ClockGroupDriverParameters(
  num:     Int = 1,
  driveFn: ClockGroupDriver.DriveFn = ClockGroupDriver.driveFromImplicitClock) {
  def drive(node: ClockGroupEphemeralNode)(implicit p: Parameters, vn: ValName): ModuleValue[RecordMap[ClockBundle]] = {
    driveFn(node, num, p, vn)
  }
}

object ClockGroupDriver {
  type DriveFn = (ClockGroupEphemeralNode, Int, Parameters, ValName) => ModuleValue[RecordMap[ClockBundle]]

  /** Drive all members of all groups from the Chisel implicit clock */
  def driveFromImplicitClock: DriveFn = { (groups, num, p, _) =>
    implicit val pp:               Parameters = p
    val dummyClockGroupSourceNode: ClockGroupSourceNode = SimpleClockGroupSource(num)
    groups :*= dummyClockGroupSourceNode
    InModuleBody { RecordMap[ClockBundle]() }
  }

  /** Drive all members of all groups from a flattened IO representation */
  def driveFromIOs: DriveFn = { (groups, num, p, vn) =>
    implicit val pp: Parameters = p
    val ioClockGroupSourceNode = ClockGroupSourceNode(List.fill(num) { ClockGroupSourceParameters() })
    groups :*= ioClockGroupSourceNode
    InModuleBody {
      val bundles = ioClockGroupSourceNode.out.map(_._1)
      val elements = bundles.flatMap(_.member.elements)
      val io = IO(Flipped(RecordMap(elements.map {
        case (name, data) =>
          name -> data.cloneType
      }: _*)))

      elements.foreach { case (name, data) => io(name).foreach { data := _ } }
      io.suggestName(vn.name)
    }
  }
}
