package org.chipsalliance.utils.ecc

import chisel3._
import chisel3.util.Cat

class ParityCode extends Code {
  def canDetect = true
  def canCorrect = false

  def width(w0:      Int) = w0 + 1
  def eccIndices(w0: Int) = Seq(w0)
  def encode(x:      UInt, poison: Bool = false.B) = Cat(x.xorR ^ poison, x)
  def swizzle(x:     UInt) = Cat(false.B, x)
  def decode(y:      UInt) = new Decoding {
    val uncorrected = y(y.getWidth - 2, 0)
    val corrected = uncorrected
    val correctable = false.B
    val uncorrectable = y.xorR
  }
}
