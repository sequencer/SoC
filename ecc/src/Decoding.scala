package org.chipsalliance.utils.ecc

import chisel3._

abstract class Decoding {
  def uncorrected: UInt

  def corrected: UInt

  def correctable: Bool

  /** If true, correctable should be ignored */
  def uncorrectable: Bool

  def error: Bool = correctable || uncorrectable
}
