package org.chipsalliance.utils.ecc

import chisel3._
import chisel3.util.ValidIO

trait CanHaveErrors extends Bundle {
  val correctable:   Option[ValidIO[UInt]]
  val uncorrectable: Option[ValidIO[UInt]]
}
