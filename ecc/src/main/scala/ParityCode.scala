package org.chipsalliance.utils.ecc

import chisel3._
import chisel3.util.Cat

class ParityCode extends Code {
  def canDetect = true
  def canCorrect = false

  def width(w0:      Int):  Int = w0 + 1
  def eccIndices(w0: Int) = Seq(w0)
  def encode(x:      UInt, poison: Bool = false.B): UInt = Cat(x.xorR ^ poison, x)
  def swizzle(x:     UInt): UInt = Cat(false.B, x)
  def decode(y:      UInt): Decoding = new Decoding {
    val uncorrected:   UInt = y(y.getWidth - 2, 0)
    val corrected:     UInt = uncorrected
    val correctable:   Bool = false.B
    val uncorrectable: Bool = y.xorR
  }
}
