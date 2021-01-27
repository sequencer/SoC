package org.chipsalliance.utils.ecc

import chisel3._

class IdentityCode extends Code {
  def canDetect = false
  def canCorrect = false

  def width(w0:         Int): Int = w0
  def eccIndices(width: Int) = Seq.empty[Int]
  def encode(x: UInt, poison: Bool = false.B): UInt = {
    require(poison.isLit && poison.litValue == 0, "IdentityCode can not be poisoned")
    x
  }
  def swizzle(x: UInt): UInt = x
  def decode(y:  UInt): Decoding = new Decoding {
    def uncorrected:   UInt = y
    def corrected:     UInt = y
    def correctable:   Bool = false.B
    def uncorrectable: Bool = false.B
  }
}
