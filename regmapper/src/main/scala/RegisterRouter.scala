// See LICENSE.SiFive for license details.

package org.chipsalliance.utils.regmapper

import chisel3.util.isPow2
import chisel3._
import diplomacy.{BundleBridgeSource, InModuleBody, LazyModule, ModuleValue}
import diplomacy.config.Parameters
import org.chipsalliance.utils.addressing.AddressSet
import org.chipsalliance.utils.dts.{Description, ResourceBindings, ResourceValue, SimpleDevice}
import org.chipsalliance.utils.prci.HasClockDomainCrossing

case class RegisterRouterParams(
  name:        String,
  compat:      Seq[String],
  base:        BigInt,
  size:        BigInt = 4096,
  concurrency: Int = 0,
  beatBytes:   Int = 4,
  undefZero:   Boolean = true,
  executable:  Boolean = false)

abstract class RegisterRouter(devParams: RegisterRouterParams)(implicit p: Parameters)
    extends LazyModule
    with HasClockDomainCrossing {

  require(isPow2(devParams.size))
  val address = Seq(AddressSet(devParams.base, devParams.size - 1))
  val concurrency: Int = devParams.concurrency
  val beatBytes:   Int = devParams.beatBytes
  val undefZero:   Boolean = devParams.undefZero
  val executable:  Boolean = devParams.executable
  val device: SimpleDevice = new SimpleDevice(devParams.name, devParams.compat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ extraResources(resources))
    }
  }
  // Allow devices to extend the DTS mapping
  def extraResources(resources: ResourceBindings): Map[String, Seq[ResourceValue]] = Map[String, Seq[ResourceValue]]()

  protected def regmap(mapping: RegField.Map*): Unit
}

abstract class IORegisterRouter[T <: Data](devParams: RegisterRouterParams, portBundle: => T)(implicit p: Parameters)
    extends RegisterRouter(devParams) {
  val ioNode: BundleBridgeSource[T] = BundleBridgeSource(() => portBundle.cloneType)
  val port:   ModuleValue[T] = InModuleBody { ioNode.bundle }
}
